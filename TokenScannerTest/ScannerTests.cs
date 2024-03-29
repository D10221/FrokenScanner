using System.Linq;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Microsoft.FSharp.Core;
using System;

namespace ScannerCsharptest
{
    using static TokenScanner.Scan;

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
            var results = scan("a+b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+");
            AreEqual(results[2], "b");
        }
        [TestMethod]
        public void Compounds()
        {
            var results = scan("a+=b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+=");
            AreEqual(results[2], "b");
        }
        [TestMethod]
        public void Compounds2()
        {
            AreEqual("a ++ = b", join(scan("a++=b"), " "));
        }
        [TestMethod]
        public void Compounds3()
        {
            AreEqual("a,+, ,+=,b", join(scan("a+ +=b"), ","));
        }
        [TestMethod]
        public void Compounds4()
        {
            AreEqual("a ++ -= b", join(scan("a++-=b"), " "));
            AreEqual("a ++ -= b", join(NoSpaces(scan("a++ -= b")), " "));
        }
        [TestMethod]
        public void CollectSpaces()
        {
            AreEqual("   ", scan("   ")[0]);
            AreEqual("\t  ", scan("\t  ")[0]);
            AreEqual("  ", scan("\r  ")[1]);
        }
        [TestMethod]
        public void FindsNewLine()
        {
            AreEqual("\n", scan(" \n ")[1]);
            AreEqual("\r\n", scan(" \r\n ")[1]);
            AreEqual("\r", scan(" \r ")[1]);
            AreEqual("\n", scan(" \n ")[1]);
            AreEqual("\r", scan(" \r \n")[1]);
            AreEqual("\n", scan(" \r \n")[3]);
            AreEqual("\r", scan(" \n \r")[3]);
            AreEqual("\n", scan(" \n \r")[1]);
        }
        [TestMethod]
        public void IsDigitTest()
        {
            AreEqual("1", scan("1a1")[0]);
            AreEqual("a1", scan("1a1")[1]);
            AreEqual("11", scan("11a1")[0]);
            AreEqual("a1", scan("11a1")[1]);
            AreEqual("1.1", scan("1.1a1")[0]);
        }
        [TestMethod]
        public void TriadScanletTest()
        {
            AreEqual("x,=>,x,==,x,=,true", join(NoSpaces(scan("x => x == x = true")), ","));
        }
        [TestMethod]
        public void TakeManyTest()
        {
            var all = scan("< <= <[ <{ <: << <-").Where(x => !string.IsNullOrWhiteSpace(x)).ToArray();
            AreEqual("<", all[0]);
            AreEqual("<=", all[1]);
            AreEqual("<[", all[2]);
            AreEqual("<{", all[3]);
            AreEqual("<:", all[4]);
            AreEqual("<<", all[5]);
            AreEqual("<-", all[6]);
        }
        private static List<string> scan(string text)
        {
            return Scan(text).Select(Fold).ToList();
        }
        private static string Fold(IEnumerable<FSharpOption<char>> values)
        {
            return values.Select(x => x.Value.ToString()).Aggregate((a, b) => a + b);
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
