using System.Linq;
using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    using static TokenScanner.ObservableScan;

    using static Assert;
    [TestClass]
    public class ObservableScannerTests
    {
        [TestMethod]
        public void ItWorks()
        {
            var results = new List<object>();
            var text = "aaa";            
            ObservableScan(text)
                .Subscribe(
                    x => results.Add(x)
                )
                .Dispose();
            IsNotNull(results.FirstOrDefault());
        }
    }
}