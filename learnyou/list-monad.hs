
-- note l1 == l2 == l3 == l4. The parens around the body of the lambda
-- exp aren't necessary, it's important to understand that that
-- this is like [1,2] >>= \n -> exp(n)
l1 = [1,2] >>= \n -> (['a','b'] >>= \ch -> return (n,ch))
f1 = ['a','b']
f2 n = \ch -> return (n,ch)
f1f2 n = f1 >>= (f2 n)
{--
f1f2 n = concat (map (f2 n) ['a','b'])
 -     = concat [f2 n 'a', f2 n 'b']
 -     = [f2 n 'a', f2 n 'b']
 - So
 - f1f2 = \n -> [f2 n 'a', f2 n 'b']
 - [1, 2] >>= f1f2
 - concat (map f1f2 [1,2])
 - concat [[f2 1 'a', f2 1 'b'], [f2 2 'a', f2 2 'b']]
 - [f2 1 'a', f2 1 'b', f2 2 'a', f2 2 'b']

--}
l2 = [1,2] >>= f1f2
l3 = do
      n <- [1,2]
      ch <- ['a','b']
      return (n,ch)

l4 = [(n, ch) | n <- [1,2], ch <- ['a', 'b']]
