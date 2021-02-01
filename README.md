# Vrcholy

Projekt sestavíme:

    stack install

Vzniknou čtyři programy:

* vrcholy-1srtm
* vrcholy-2potopa
* vrcholy-3geonames
* vrcholy-4gpx

Programy na sebe navazují.

Všechny programy přijímají stejný jediný parametr a to adresář s daty. Struktura v tomto adresáři je pevná:

    0srtm/
        ..
        N49E016.hgt
        ..
    1srtmByElevation/
         ..
         530
         531
         532
         ..
    2vrcholy.txt
    3geonames/
        ..
        geonames-N49E16.txt
        ..
    4gpx.gpx

V repozitoři je adresář `data` obsahující vzorek vstupních výškových dat a také zrpacované ty výsledky, jejichž výpočet byl dlouhý.

Programy jsou očíslovány.
* Každý program vyrábí adresář nebo soubor se stejným číslem, jaké je číslo programu.
* Každý program čte z některého z předchozích čísel.

# Programy

## vrycholy-1srtm
Transponuje výšková data pro lepší zpracování při zatápění reliéfu.

    0srtm --> 1srtmByElevation

Do `0srtm` ručně nasypeme výšková srtm data získaná z geogetu nebo stažené přímo jako výšková srtm data.V obdélnící po zeměpisných stupních, granularita po třech úhlových vteřinách. 

V repozitoři je jediný soubor, aby programy byly funkční bez nutnosti vyhledávat další data.

**Výstup:** Výšková data transponovaná dle nadmořské výšky, s tím se pak lépe pracuje.

## vrcholy-2potopa

Toto je hlavní program pro určení vrcholů, klíčových sedel a mateřských vrcholů.

    1srtmByElevation --> 2vrcholy.txt

Program postupně zatápí terén, běží velmi dlouho, hodiny a možná i dny. Neukládá se mezistav, musí doběhnout celý.

Protože zpracování trvá, se soubor `2vrcholy.txt` v repozitoři.

## vrcholy-3geoname

Program pro nalezené vrcholy i klíčová sedla vyhledává názvy na Internetu (http://api.geonames.org/findNearbyJSON).

    2vrcholy.txt + 3geonames --> 3geonames

Je restartovatelný. Na začátku načte, ke kterým vrcholům jsme něco dotáhnuli a co není dotaženo, dotáhne.
Trvá poměrně dlouho, mnohod dnů, free verze API má omezený počet volání.
Program je možné přeskočit, pak ovšem nebudou vrcholy pojmenované.

Protože zpracování trvá, jsou soubory `3geonames` v repozitoři.

## vrcholy-4gpx
Zpracuje všechny vrcholy do formátu `gpx` pro geoget či geokuk. Pokud je pro nějaký vrchol či sedlo nalezeno jméno, použije ho, jinak vytvoří vrchol bezejmenný.

    2vrcholy.txt + 3geonames --> 4gpx.gpx 







