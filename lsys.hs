--Tp3 Caron Matthieu et Cojez Arnaud
import Graphics.Gloss

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]
type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
type EtatDessin = (EtatTortue, Path)              
       
--Question 1
motSuivant :: Regles -> Mot -> Mot
motSuivant f [] = []
motSuivant f (x:xs) = (f x)++(motSuivant f xs)

motSuivant' :: Regles -> Mot -> Mot
motSuivant' f xs = concatMap f xs


--3e todo
motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' f x = x

--Question 2
regleFloc '+' = ['+']
regleFloc '-' = ['-']
regleFloc f = f:'-':f:'+':'+':f:'-':f:[]

--Question 3
lsysteme :: Axiome -> Regles -> LSysteme

lsysteme a r =  if (motSuivant r a == a) then [a]  
				else iterate (motSuivant r) a 

--Question 4
etatInitial :: Config -> EtatTortue
etatInitial (a,_,_,_,_) = a

longueurPas :: Config -> Float
longueurPas (_,a,_,_,_) = a

facteurEchelle :: Config -> Float
facteurEchelle (_,_,a,_,_) = a

angle :: Config -> Float
angle (_,_,_,a,_) = a

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,a) = a

--Question 5 
avance :: Config -> EtatTortue -> EtatTortue
avance conf ((x,y),cap) = ((x+(longueurPas conf)*(cos cap),y+(longueurPas conf)*(sin cap)),cap)

--Question 6
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche conf (point,cap) = (point,(cap + angle conf))

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite conf (point,cap) = (point,(cap - angle conf))

--Question 7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue _ [] = []
filtreSymbolesTortue conf (x:xs) = if any ((==) x) (symbolesTortue conf) then
								   		x:filtreSymbolesTortue conf xs
								   else 
								   		filtreSymbolesTortue conf xs
--Question 8
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin						 
interpreteSymbole conf (etat,path) 'F' = (avance conf etat,(fst (avance conf etat)):path)
interpreteSymbole conf (etat,path) '+' = (tourneAGauche conf etat,(fst (tourneAGauche conf etat)):path)
interpreteSymbole conf (etat,path) '-' = (tourneADroite conf etat,(fst (tourneADroite conf etat)):path)


--Question 9
interpreteMot :: Config -> Mot -> Picture
interpreteMot conf mot = 






























