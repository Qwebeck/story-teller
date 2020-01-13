# Generator historii w prologu
Program jest podzielony na 4 pliki:

1. `story_teller.pl` - plik główny. W pliku mieści się tylko jeden predykat: `gen_s/0`. Jedynym zadaniem predykatu jest generowanie i wypisywanie historii. 
2. `utils.pl` - plik pomocniczy. W pliku są umieszczone funkcje pomocnicze, które są wykorzystywane w innych częściach programu.
3. `grammar.pl` - plik który opisuje gramatykę tworzonej historii.
4. `lexic.pl` - baza wiedzy generatora.
# Przykładowe historii
Na razie są generowane tylko 2 scenariusze:
## Darowanie prezentu
```
It is a magical teatime under a famous mountains . A  John Snow was giggling at hat in an ancient cave . He was hopeful because the life is great . Suddenly hopeful Winston comes  in cave : 
" Hello , John Snow . Success is not final, failure is not fatal: it is the courage to continue that counts  . It is a good day for you! . I bring  a a nice goblet . . 
" Great, thank you! Winston . I very appreciate this goblet "-said John Snow . 
" Glad to hear it. John Snow .  "-answer Winston 
The end.
```
## Prośba o pomoc
```
It is a rainy teatime in a galactic London . A crazy John Snow was roaring at magic at a quiet cabinet . He was feared because his leg was broken . Suddenly feared Winston comes  at cabinet : 
" Hello , John Snow . Success is not final, failure is not fatal: it is the courage to continue that counts  . I need you! . I don't know what to do Foltest attacks London . He wants to  ruin the King . He will be stopped only if you will give a Ghost . Nobody other can't do that . 
" I don't care Winston . Sorry,I can't help you "-said John Snow . 
" Doesn't matter John Snow .  "-answer Winston 
John Snow tried to help  Winston to save  London  but he met with failure 
```
# Uruchomienie
Żeby przetestować program należy pobrać repozytorium, zaimportować plik `story_teller.pl` i wywołać predykat `gen_s/0`.
```
?- ['story_teller.pl'].
?- gen_s.
```

# Działanie
Działanie programu rozpoczyna się z predykatu `gen_s/0`, który wywołuje predykat `story/2`. Predykat ten odpowiada za wybór losowego predykatu, do tworzenia wstępu do historii. Na razie losowości nie ma dlatego, że istnieje tylko jeden predykat, tworzący wstępy, ale nic nie stoi na przeszkodzie do tego, żeby opisać kolejne. W tym przypadku, predykat  `random_grammar_clause_v2/4`, wybierze i uruchomi jeden z nich. Dalej będzie opisane działanie na przykładzie predykatu `inroduction(start_in_place)`, który odpowiada za to, żeby historia rozpoczynała się z opisu jakiegoś miejsca.
```prolog
introduction(start_in_place) -->
    place_descr(GlobalMood, Location),
    hero_descr(GlobalMood, Location, Hero, ConcretePlace, HeroMood),
    {
      event_number(N),
      random_grammar_clause_v2(event,
                               N,
                               
                               [ _,
                                 GlobalMood,
                                 Location,
                                 Hero,
                                 ConcretePlace,
                                 HeroMood
                               ],
                               Event)
	},
	Event.		
```
W pierwszej linijce widzimy predykat `place_descr/2`. Oba parametry są przez niego zwracane i mają wpływ na dalszy ciąg historii. Parametr `GloabalMood` - decyduje o tym, jaki humor będzie miał główny bohater, a parametr `Location`, o tym w jakiej lokacji będzie się odbywać historia. Parametry te są wybierany z bazy wiedzy w sposób losowy. 
Potem , podajemy te parametry do predykatu `hero_descr/5`. Predykat ten zaczyna tworzyć opis bohatera, tego co on robi i tego gdzie się znajduje, na podstawie parametrów które przekazaliśmy do niego i zwraca nowe parametry które będą miały wpływ na dalszy przebieg historii: `Hero,ConcretePlace,HeroMood`.
Następnie jest losowany i wywoływany predykat, który opisuje zdarzenie i przyjmuje jako argumenty wcześniej wygenerowane parametry. Za pomocą podobnych procesów: losowania i budowania związków jest tworzona reszta historii. 

