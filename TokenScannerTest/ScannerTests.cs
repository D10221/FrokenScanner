using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.Scanner;
    using static Assert;
    [TestClass]
    public class ScannerTests
    {
        [TestMethod]
        public void ItDoesntCrash()
        {
            var result = Scan("");
        }
        [TestMethod]
        public void Basics()
        {
            var results = Scan("a+b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+");
            AreEqual(results[2], "b");
        }
        [TestMethod]
        public void Compunds()
        {
            var results = Scan("a+=b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+=");
            AreEqual(results[2], "b");
        }
    }
}
