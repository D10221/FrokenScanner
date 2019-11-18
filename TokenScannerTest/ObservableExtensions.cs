using System;
using System.Collections.Generic;

namespace ScannerCsharptest
{
    public static class ObservableExtensions
    {
        public static IDisposable Subscribe<T>(this IObservable<T> observable, Action<T> onNext, Action onCompleted = null, Action<Exception> onError = null)
        {
            return observable.Subscribe((Observer<T>)onNext);
        }

        public static List<R> ToList<T, R>(this IObservable<T> observable, Func<T, R> f)
        {
            var list = new List<R>();
            IDisposable disposable = null;

            void dispose() => disposable?.Dispose();

            void onCompleted() => dispose();

            disposable = observable.Subscribe(
                    value =>
                    {
                        list.Add(f(value));
                    },
                    onCompleted
                    );
            return list;
        }

    }
}