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
        public void Compunds()
        {
            var results = Scan("a+=b");
            AreEqual(results[0], "a");
            AreEqual(results[1], "+=");
            AreEqual(results[2], "b");
        }
        [TestMethod]
        public void Compunds2()
        {            
            AreEqual("a ++ = b", join(Scan("a++=b"), " "));
        }
        [TestMethod]
        public void Compunds3()
        {            
            AreEqual("a,+, ,+=,b", join(Scan("a+ +=b"), ","));
        }
        [TestMethod]
        public void Compunds4()
        {            
            AreEqual("a ++ -= b", join(Scan("a++-=b"), " "));
            AreEqual("a ++ -= b", join(NoSpaces(Scan("a++ -= b")), " "));
        }
        static IEnumerable<string> NoSpaces(IEnumerable<string> input){
            return input.Where(x=> !string.IsNullOrWhiteSpace(x));
        }
        static string join(IEnumerable<string> input, string separator = "")
        {
            return input.Aggregate((a, b) => a + separator + b);
        }
    }
}
