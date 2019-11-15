using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
  [TestClass]
  public class TokenScannerTests
  {
    [TestMethod]
    public void ItDoesntCrash()
    {
      // Microsoft.FSharp.Collections.FSharpList<Microsoft.FSharp.Core.FSharpOption<string>>
      var scanner = TokenScanner.Scanner.Scanner();
      var scanned = scanner.Invoke("");
    }
  }
}
