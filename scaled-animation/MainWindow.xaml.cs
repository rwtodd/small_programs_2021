using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Threading;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace scaled_animation
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private int ctr;
        private readonly WriteableBitmap wbm;
        public MainWindow()
        {
            InitializeComponent();
            ctr = -1;
            wbm = new(160, 200, 96, 96, PixelFormats.Indexed4, BitmapPalettes.Gray16);
            AgiScreen.Source = wbm;
            var dtm = new DispatcherTimer() { Interval = TimeSpan.FromMilliseconds(16)  };
            dtm.Tick += Dtm_Tick;
            dtm.Start();
        }

        private void Dtm_Tick(object sender, EventArgs e)
        {
            ctr += 1;
            if (ctr >= wbm.PixelHeight)
            {
                (sender as DispatcherTimer)?.Stop();
                return;
            }

            try
            {
                wbm.Lock();
                unsafe
                {
                    int stride = wbm.BackBufferStride;
                    Span<byte> sb = new Span<byte>(wbm.BackBuffer.ToPointer(), stride * wbm.PixelHeight);
                    int row = this.ctr;
                    int color = this.ctr % 16;
                    int sbIdx = row * stride;
                    for(int col = 0; col < stride; col++)
                    {
                        sb[sbIdx++] = (byte)(color | (color << 4));
                    }

                    // Specify the area of the bitmap that changed.
                    wbm.AddDirtyRect(new Int32Rect(0, row, wbm.PixelWidth, 1));
                }
            }
            finally
            {
                wbm.Unlock();
            }
        }
    }
}
