using Microsoft.FSharp.Core;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace ScannerCsharptest
{
    public static class FSharpeyExtensions
    {
        public static TResult Invoke<TResult>(this FSharpFunc<Unit, TResult> func)
        {
            return func.Invoke(null);
        }
        public static bool IsNone<T>(this FSharpOption<T> option){
          return option == null || Equals(option.Value, default);
        }
    }
}
