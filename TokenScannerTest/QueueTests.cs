using System;
using Microsoft.FSharp.Core;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.Queue;
    using static Assert;
    using static FSharpeyExtensions;
    [TestClass]
    public class QueueTests
    {
        [TestMethod]
        public void QueueTest()
        {
            var _next = Queue("");
            Func<FSharpOption<char>> peek = () => _next.Invoke(false);
            Func<FSharpOption<char>> next = () => _next.Invoke(true);
            AreEqual(peek(), next());
        }
        ///<summary>
        /// Doesn't throw
        ///</summary>
        [TestMethod]
        public void QueueTest1()
        {
            var _next = Queue("");
            Func<FSharpOption<char>> peek = () => _next.Invoke(false);
            Func<FSharpOption<char>> next = () => _next.Invoke(true);

            var a = peek();
            var b = peek();
            var c = peek();
            AreEqual(a, b);
            AreEqual(b, c);
            AreEqual(a, c);
            a = next();
            b = next();
            c = next();
            AreEqual(a, b);
            AreEqual(b, c);
            AreEqual(a, c);
        }
        ///<summary>
        /// Doesn't throw
        ///</summary>
        [TestMethod]
        public void QueueTest2()
        {
            var _next = Queue("a");
            Func<FSharpOption<char>> peek = () => _next.Invoke(false);
            Func<FSharpOption<char>> next = () => _next.Invoke(true);

            AreEqual(peek.Invoke().Value, 'a');//index never moved, future is 'a'
            AreEqual(peek.Invoke().Value, 'a');//index never moved, again!
            AreEqual(next.Invoke().Value, 'a');//moved, present is 'a'
            // Its seems Fsharp returns NUll
            IsTrue(IsNone(next.Invoke()));
            IsTrue(IsNone(peek.Invoke()));
        }
        ///<summary>
        /// Doesn't throw
        ///</summary>
        [TestMethod]
        public void QueueTest3()
        {
            var _next = Queue("ab");
            Func<FSharpOption<char>> peek = () => _next.Invoke(false);
            Func<FSharpOption<char>> next = () => _next.Invoke(true);

            AreEqual(peek.Invoke().Value, 'a');
            AreEqual(next.Invoke().Value, 'a');

            AreEqual(peek.Invoke().Value, 'b');
            AreEqual(next.Invoke().Value, 'b');

            IsTrue(IsNone(next.Invoke()));
            AreEqual(peek.Invoke(), null);
            IsTrue(IsNone(peek.Invoke()));
        }
    }
}
