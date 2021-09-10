using System;

namespace virtual_methods
{
    class Program
    {
	/* *************************************************************** */
	/* Interface methods can't be marked virtual... I tried. */
	internal interface ITest {
		int CalcInt();
		int TheNumber { get; }
	}

	/* strange that CalcInt must be marked virtual here if we want to
	override it in Test2 below.  You would think that it would already
	be virtual since it originates in the ITest interface. 

	Note: properties work the exact same way methods do, at least in terms
	of the 'virtual public' and 'override public' syntax.
	*/
	internal class Test1 : ITest {
		virtual public int CalcInt() => 5;
		virtual public int TheNumber { get => 666; }
	}

	internal class Test2 : Test1 {
		override public int CalcInt() => 100;
		override public int TheNumber { get => 999; }
	}
	
        static void Main(string[] args)
        {
		ITest t1 = new Test1();
		Console.WriteLine($"Test1 gives {t1.CalcInt()} and {t1.TheNumber}");
		t1 = new Test2();
		Console.WriteLine($"Test2 gives {t1.CalcInt()} and {t1.TheNumber}");
        }
    }
}
