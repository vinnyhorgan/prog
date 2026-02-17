#!/usr/bin/perl
use vars qw($x);  # scope dinamico

$x = 10;

sub stampa_x {
    print "x vale: $x\n";
}

sub chiamante_A {
    local $x = 100;  # 'local' usa scope dinamico
    stampa_x();
}

sub chiamante_B {
    local $x = 200;
    stampa_x();
}

stampa_x();      # x vale: 10
chiamante_A();   # x vale: 100
chiamante_B();   # x vale: 200
