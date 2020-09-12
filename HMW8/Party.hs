module Party where


import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empName = name ,empFun=fun}) (GL ls f) = (GL (e:ls) (f+fun)) 

conc :: GuestList -> GuestList -> GuestList
conc (GL l1 f1) (GL l2 f2) = (GL (l1++l2) (f1+f2))

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend = conc 

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ _) b@(GL _ _) 
                    | a<b = b
                    | a>=b = a


treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f ac (Node{rootLabel =label ,subForest = ls}) = f label treemap
    where treemap = map (treeFold f ac) ls


nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glWithBoss', glWithoutBoss')
    where glWithBoss'        = glCons boss $ mconcat glWithoutBoss
          glWithoutBoss'     = mconcat $ map (uncurry $ moreFun) gls
          (_, glWithoutBoss) = unzip gls

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry $ moreFun) . treeFold nextLevel (emptyGuestList, emptyGuestList)
    where emptyGuestList = mempty :: GuestList

main :: IO()
main = readFile "company.txt" >>= (putStr . unlines . prepareOutput . read)

prepareOutput :: Tree Employee -> [String]
prepareOutput tree = strFun : names
    where strFun = "Total fun: " ++ show fun
          names = map empName emps
          GL emps fun = maxFun tree
