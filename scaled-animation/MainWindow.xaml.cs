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
        private readonly uint[] palette = new uint[16] { 
            0x00, 0xaa, 0xaa00, 0xaaaa, 0xaa0000, 0xaa00aa, 0xaa5500, 0xaaaaaa,
            0x555555, 0x5555ff, 0x55ff55, 0x55ffff, 0xff5555, 0xff55ff, 0xffff55, 0xffffff
        };

        public MainWindow()
        {
            InitializeComponent();
            ctr = -1;
            wbm = new(160, 200, 96, 96, PixelFormats.Bgr32, null);
            AgiScreen.Source = wbm;
            var dtm = new DispatcherTimer() { Interval = TimeSpan.FromMilliseconds(1000/60) };
            dtm.Tick += Dtm_Tick;
            dtm.Start();
        }

        private void Dtm_Tick(object? sender, EventArgs e)
        {
            ctr = ctr + 1;
            if(ctr > 15) ctr = 0;

            try
            {
                wbm.Lock();
                unsafe
                {
                    int pIdx = ctr;
                    int stride = wbm.BackBufferStride >> 2;
                    Span<uint> sb = new Span<uint>(wbm.BackBuffer.ToPointer(), stride * wbm.PixelHeight);
                    for (int row = 0; row < wbm.PixelHeight; row++)
                    {
                        int sbIdx = row * stride;
                        for (int col = 0; col < wbm.PixelWidth; col++)
                        {
                            sb[sbIdx++] = palette[pIdx++];
                            if(pIdx > 15) pIdx = 0;
                        }
                        ++pIdx;
                        if(pIdx > 15) pIdx = 0;
                    }

                    // Specify the area of the bitmap that changed.
                    wbm.AddDirtyRect(new Int32Rect(0, 0, wbm.PixelWidth, wbm.PixelHeight));
                }
            }
            finally
            {
                wbm.Unlock();
            }
        }
    }
}
