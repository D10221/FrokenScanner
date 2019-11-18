using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;

namespace ScannerCsharptest
{
    public class Observer<T> : IObserver<T>
    {
        Action<T> onNext;
        Action<Exception> onError;
        Action onCompleted;
        public Observer(Action<T> onNext, Action<Exception> onError = null, Action onCompleted = null)
        {
            this.onNext = onNext;
            this.onError = onError;
            this.onCompleted = onCompleted;
        }
        public void OnCompleted()
        {
            this.onCompleted?.Invoke();
        }

        public void OnError(Exception error)
        {
            if (this.onError != null)
                this.onError.Invoke(error);
            else throw error;
        }

        public void OnNext(T value)
        {
            this.onNext.Invoke(value);
        }
        public static implicit operator Observer<T>(Action<T> action)
        {
            return new Observer<T>(action);
        }
    }
}