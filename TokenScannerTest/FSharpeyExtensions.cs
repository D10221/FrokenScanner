using Microsoft.FSharp.Core;

namespace ScannerCsharptest
{
    public static class FSharpeyExtensions
    {
        public static TResult Invoke<TResult>(this FSharpFunc<Unit, TResult> func)
        {
            return func.Invoke(null);
        }
        public static bool IsNone<T>(FSharpOption<T> option){
          return option == null || Equals(option.Value, default);
        }
        public static T Value<T>(FSharpOption<T> option){
          return IsNone(option) ? default : option.Value;
        }
    }
}
