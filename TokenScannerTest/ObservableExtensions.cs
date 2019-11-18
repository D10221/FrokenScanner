using System;

namespace ScannerCsharptest
{
    public static class ObservableExtensions
    {
        public static IDisposable Subscribe<T>(this IObservable<T> observable, Action<T> observer)
        {
            return observable.Subscribe((Observer<T>)observer);
        }
    }
}