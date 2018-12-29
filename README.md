# <div align="center">Dagless 🕶 📚</div>

> _The doors of Darkplace were opened. Not the **literal** doors of the building –
> most of which were closed – but evil doors, dark doors, doors to the beyond.
> Doors that were hard to shut because they were abstract, and didn't have
> handles. They were more like portals, really. From this day on, I'd have to
> fight these forces of darkness... and deal with the burden of day-to-day
> admin._ - [Dr. Rick Dagless,
  M.D.](https://en.wikipedia.org/wiki/Garth_Marenghi%27s_Darkplace)

Dagless is a DSL for constructing heterogenous computations representable as
[DAGs](https://en.wikipedia.org/wiki/Directed_acyclic_graph). Specifically, it
uses an [IxStateT
transformer](http://hackage.haskell.org/package/indexed-extras/docs/Control-Monad-Indexed-State.html)
to keep track of updates to the graph. When combined with the fabulous
[`do-notation`](http://hackage.haskell.org/package/do-notation) package,
however, this implementation detail is neatly hidden away!

---

See the [Test](https://github.com/i-am-tom/dagless/tree/master/test/Test)
directory for fully-worked examples!

```haskell
main collection = compute' do
  mass         <- fetch @Mass
  acceleration <- fetch @Acceleration

  force <- using (mass, acceleration) $ \(m, a) -> do
    Mass         m' <- m
    Acceleration a' <- a

    pure (Force (m' * a'))

  displacement <- fetch @Displacement

  using (force, displacement) $ \(f, d) -> do
    Force        f' <- f
    Displacement d' <- d

    pure (Energy (f' * d'))
```
