# Typage

Le typage en Stellogen illustre des principes majeurs de syntaxe transcendantale
où les types sont définis comme des ensembles de *tests* eux-mêmes définis
avec des constellations. La vérification de type implique le passage de tous
les tests associés à un type.

Les types sont donnés par une galaxie où chaque champ représent un test à faire
passer :

```
t = galaxy
  test1: @-f(X) $ok; -g(X).
  test2: @-g(X) $ok; -f(X).
end
```

Une galaxie/constellation *testée* doit donc être confrontée à tous les tests
définis par la galaxie ci-dessous pour être de type `t`.

> Un type avec un unique test peut être défini par une simple constellation.

Il nous reste cependant à définir ce que signifient :
- être confronté à un test;
- passer un test avec succès.

> Conformément à la correspondance de Curry-Howard, les types peuvent être vus
comme des formules ou spécifications. On pourrait aussi étendre cela à la
proposition de Thomas Seiller de voir les algorithmes comme des spécifications
plus sophistiquées.

## Checkers

Il faut définir des *checkers* qui sont des galaxies "juges" définissant :
- un champ `interaction` contenant nécessairement un token `{tested}` et un
autre token `{tested}` expliquant comment faire interagir un testé et un test;
- un champ `expect` indiquant quel est le résultat attendu par l'interaction.

Par exemple :

```
checker = galaxy
  interaction: {tested} {test}.
  expect: $ok.
end
```

## Typage par test

On peut ensuite faire précéder les définitions par une déclaration de type :

```
c :: t [checker].
c = +f($0) +g($0).
```

La constellation `c` passe bel et bien les tests `test1` et `test2` de `t`.
Il n'y a donc aucun problème. On dit donc que `c` est de type `t` ou que `c`
est une *preuve* de la formule `t`.

Cependant, si nous avions une constellation qui ne passait pas les tests, comme
:

```
c :: t [checker].
c = +f($0).
```

Nous aurions eu un message d'erreur nous indiquant qu'un test n'est pas passé.


Voici un type avec un unique test pour une représentation naïve des entiers
naturels :

```
nat =
  -nat($0) $ok;
  -nat($s(N)) +nat(N).

0 :: nat [checker].
0 = +nat($0).

1 :: nat [checker].
1 = +nat($s($0)).
```

On peut aussi omettre de préciser le checker. Dans ce cas, le checker par
défaut est :

```
checker = galaxy
  interaction: {tested} {test}.
  expect: $ok.
end
```

Cela nous permet d'écrire :

```
0 :: nat.
0 = +nat($0).

1 :: nat.
1 = +nat($s($0)).
```
