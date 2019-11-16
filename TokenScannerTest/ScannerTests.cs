using System.Linq;
using System.Collections.Generic;
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
        public void Compounds()
        {
            var results = Scan("a+=b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+=");
            AreEqual(results[2], "b");
        }
        [TestMethod]
        public void Compounds2()
        {
            AreEqual("a ++ = b", join(Scan("a++=b"), " "));
        }
        [TestMethod]
        public void Compounds3()
        {
            AreEqual("a,+, ,+=,b", join(Scan("a+ +=b"), ","));
        }
        [TestMethod]
        public void Compounds4()
        {
            AreEqual("a ++ -= b", join(Scan("a++-=b"), " "));
            AreEqual("a ++ -= b", join(NoSpaces(Scan("a++ -= b")), " "));
        }
        [TestMethod]
        public void CollectSpaces()
        {
            AreEqual("   ", Scan("   ")[0]);
            AreEqual("\t  ", Scan("\t  ")[0]);
            AreEqual("  ", Scan("\r  ")[1]);
        }
        [TestMethod]
        public void FindsNewLine()
        {           
            var all = Scan(" \n ");                        
            AreEqual("\n", all[1]);                        
            AreEqual("\r\n", Scan(" \r\n ")[1]);
            AreEqual("\r", Scan(" \r ")[1]);
            AreEqual("\n", Scan(" \n ")[1]);
            AreEqual("\r", Scan(" \r \n")[1]);
            AreEqual("\n", Scan(" \r \n")[3]);
            AreEqual("\r", Scan(" \n \r")[3]);
            AreEqual("\n", Scan(" \n \r")[1]);
        }

        [TestMethod]
        public void TiadScanletTest()
        {
            AreEqual("x,=>,x,==,x,=,true", join(NoSpaces(Scan("x => x == x = true")), ","));
        }
        static IEnumerable<string> NoSpaces(IEnumerable<string> input)
        {
            return input.Where(x => !string.IsNullOrWhiteSpace(x));
        }
        static string join(IEnumerable<string> input, string separator = "")
        {
            return input.Aggregate((a, b) => a + separator + b);
        }
    }
}
