%%%% -*- Mode: Prolog -*-

%%%% family.pl --
%%%% Family tree example from "Art of Prolog" plus the "Mahabharata".

%%%% Males and females.

%%% male/1

male(terach).
male(abraham).
male(nachor).
male(haran).
male(isaac).
male(lot).
male(esau).
male(jacob).
male(bethuel).
male(laban).
male(benjamin).
male(joseph).

male(shantanu).
male(bhishma).
male(parashara).
male(chitrangada).
male(vichitravirya).

male(vyasa).

male(pandu).
male(dhritarashtra).
male(vidura).

male(yudhishtira).
male(bhima).
male(arjuna).
male(nakula).
male(sahadeva).
male(vyasa).

male(karna).

male(dharma).
male(vayu).
male(indra).
male(surya).

male(duryodhana).
male(dushasana).
male(vikarna).
male(sukarna).

male(destiny).
male(destruction).
male(dream).
male(desire).


%%% female/1

female(sarah).
female(milcah).
female(yiscah).
female(rebecca).
female(rachel).


female(ganga).

female(satyavati).

female(amba).
female(ambika).
female(ambalika).
female('ambalika and ambika\'s maid').

female(gandhari).
female(kunti).
female(madri).

female(death).
female(despair).
female(desire).
female(delirium).


%%% father/2 --
%%% father(X, Y) means 'X is father of Y'.

father(terach, abraham).
father(terach, nachor).
father(terach, haran).
father(abraham, isaac).
father(haran, lot).
father(haran, milcah).
father(haran, yiscah).
father(nachor, bethuel).
father(isaac, esau).
father(isaac, jacob).
father(jacob, benjamin).
father(jacob, joseph).

father(shantanu, bhishma).
father(shantanu, chitrangada).
father(shantanu, vichitravirya).

father(parashara, vyasa).

father(vyasa, dhritarashtra).
father(vyasa, pandu).
father(vyasa, vidura).

father(dharma, yudhistira).
father(dharma, bhima).
father(dharma, arjuna).

father(vayu, yudhistira).
father(vayu, bhima).
father(vayu, arjuna).

father(indra, yudhistira).
father(indra, bhima).
father(indra, arjuna).

father(ashwini, sahadeva).
father(ashwini, nakula).

father(dhritarashtra, duryodhana).
father(dhritarashtra, dushasana).
father(dhritarashtra, vikarna).
father(dhritarashtra, sukarna).

father(surya, karna).

father(dream, orpheus).


%%% mother/2
%%% mother(X, Y) means 'X is mother of Y'.

mother(sarah, isaac).
mother(rebecca, esau).
mother(rebecca, jacob).
mother(rachel, benjamin).
mother(rachel, joseph).
mother(milcah, bethuel).

mother(satyavati, vyasa).

mother(ambika, dhritarashtra).
mother(ambalika, pandu).
mother('ambalika and ambika\'s maid', vidura).

mother(kunti, yudhishtira).
mother(kunti, bhima).
mother(kunti, arjuna).
mother(kunti, karna).

mother(madri, nakula).
mother(madri, sahadeva).

mother(gandhari, duryodhana).
mother(gandhari, dushasana).
mother(gandhari, vikarna).
mother(gandhari, sukarna).

%%%% end of file -- families_facts.pl --
