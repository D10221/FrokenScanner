using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.Scanner;
    using static Assert;
    using static FSharpeyExtensions;
    [TestClass]
    public class ScannerTests
    {
        [TestMethod]
        public void ItDoesntCrash()
        {
            // Microsoft.FSharp.Collections.FSharpList<Microsoft.FSharp.Core.FSharpOption<string>>
            var result = Scan("");
        }
    }
}
