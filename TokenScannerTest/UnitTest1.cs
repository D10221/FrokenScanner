using System.Collections.Generic;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    [TestClass]
    public class UnitTest1
    {
        [TestMethod]
        public void TestMethod1()
        {
            FSharpList<FSharpOption<string>> x = TokenScanner.Scanner.Scan("SSSSS");
        }
    }
}
