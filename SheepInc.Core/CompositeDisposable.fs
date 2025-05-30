namespace SheepInc.Core

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Collections.Immutable

// Inspired by https://github.com/dotnet/reactive/blob/fefe75993de542bc8dc6e01287fce0956a6770ab/Rx.NET/Source/src/System.Reactive/Disposables/CompositeDisposable.cs

type CompositeDisposable private(children: List<IDisposable>) =
    let mutable disposed = false
    let children = children
    let childrenLock = obj()
    
    member inline private this.GuardDisposed () = if disposed then raise (ObjectDisposedException(nameof(CompositeDisposable)))
    
    new() = new CompositeDisposable(List<_>())
    new(children: IDisposable seq) = new CompositeDisposable(List<_>(children))
    
    member this.Add disposable =
        this.GuardDisposed ()
        lock childrenLock (fun () -> children.Add disposable)
    
    member this.Remove disposable =
        this.GuardDisposed ()
        lock childrenLock (fun () -> children.Remove disposable)
    
    abstract member Dispose : disposing:bool -> unit
    default this.Dispose disposing =
        if disposing then
            if not disposed then
                lock childrenLock (fun () ->
                    for child in children do
                        child.Dispose ()
                    children.Clear ()
                )
            disposed <- true
    
    member this.Dispose () = this.Dispose true
    
    interface IDisposable with
        override this.Dispose () = this.Dispose ()
    