using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.Queue;
    using static Assert;

    [TestClass]
    public class QueueTests
    {
        [TestMethod]
        public void QueueTest()
        {
            var queue = Queue("");
            var peek = queue.Item1;
            var next = queue.Item2;
            AreEqual(peek.Invoke(null), next.Invoke(null));
        }
        ///<summary>
        /// Doesn't throw
        ///</summary>
        [TestMethod]
        public void QueueTest1()
        {
            var queue = Queue("");
            var peek = queue.Item1;
            var next = queue.Item2;
            var a = peek.Invoke();
            var b = peek.Invoke();
            var c = peek.Invoke();
            AreEqual(a, b);
            AreEqual(b, c);
            AreEqual(a, c);
            a = next.Invoke();
            b = next.Invoke();
            c = next.Invoke();
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
            var queue = Queue("a");
            var peek = queue.Item1;
            var next = queue.Item2;

            AreEqual(peek.Invoke().Value, 'a');//index never moved, future is 'a'
            AreEqual(peek.Invoke().Value, 'a');//index never moved, again!
            AreEqual(next.Invoke().Value, 'a');//moved, present is 'a'
            // Its seems Fsharp returns NUll
            IsTrue(next.Invoke().IsNone());
            // again!
            IsTrue(next.Invoke().IsNone());
        }
        ///<summary>
        /// Doesn't throw
        ///</summary>
        [TestMethod]
        public void QueueTest3()
        {
            var queue = Queue("a");
            var peek = queue.Item1;
            var next = queue.Item2;

            AreEqual(peek.Invoke().Value, 'a');//index never moved, future is 'a'
            AreEqual(peek.Invoke().Value, 'a');//index never moved, again!
            AreEqual(next.Invoke().Value, 'a');//moved, present is 'a'
            // Its seems Fsharp returns NUll
            IsTrue(next.Invoke().IsNone());
            // again!
            IsTrue(next.Invoke().IsNone());
        }
    }
}
