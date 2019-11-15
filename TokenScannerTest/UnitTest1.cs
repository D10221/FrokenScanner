using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.Scanner;
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {            
            // Microsoft.FSharp.Collections.FSharpList<Microsoft.FSharp.Core.FSharpOption<string>>
            var x = Scan("SSSSS"); 
        }
    }
}
