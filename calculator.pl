#!/usr/bin/env perl
use strict;
use warnings;

my %vars;

print "Perl Basit Hesap Makinesi. Çıkmak için 'quit' yazın.\n";

while (1) {
    print "> ";
    my $line = <STDIN>;
    exit(0) if !defined $line; # Ctrl-D durumu
    chomp $line;
    last if $line eq 'quit';

    # Değişken atama kontrolü (geçerli isimler için)
    if ($line =~ /^\s*([a-zA-Z_]\w*)\s*=\s*(.+?)\s*$/) {
        my $var_name = $1;
        my $expr = $2;
        my $value = evaluate_expression($expr, \%vars);
        if (defined $value) {
            $vars{$var_name} = $value;
        } else {
            print "Hata: Geçersiz ifade.\n";
        }
    } else {
        # Doğrudan ifade değerlendirme
        my $value = evaluate_expression($line, \%vars);
        print "$value\n" if defined $value;
    }
}

sub evaluate_expression {
    my ($expr, $vars) = @_;
    my @tokens = split(/([+\-*\/()]|\d+|\w+)/, $expr);
    @tokens = grep { !/^\s*$/ } @tokens; # Boşlukları temizle
    
    return unless @tokens;

    # Operatör önceliği için iki aşamalı işlem
    my @output;
    my @stack;
    my %prec = ('*' => 3, '/' => 3, '+' => 2, '-' => 2);

    # Shunting-yard algoritması
    for my $token (@tokens) {
        if ($token =~ /^\d+$|^\w+$/) {
            push @output, $token;
        } elsif ($token =~ /[+\-*\/]/) {
            while (@stack && $prec{$stack[-1]} >= $prec{$token}) {
                push @output, pop @stack;
            }
            push @stack, $token;
        }
    }
    push @output, reverse @stack;

    # Postfix değerlendirme
    my @eval_stack;
    for my $token (@output) {
        if ($token =~ /^\d+$/) {
            push @eval_stack, $token;
        } elsif ($token =~ /^\w+$/) {
            if (exists $vars->{$token}) {
                push @eval_stack, $vars->{$token};
            } else {
                print "Tanımsız değişken: $token\n";
                return;
            }
        } else {
            my $b = pop @eval_stack;
            my $a = pop @eval_stack;
            if (!defined $a || !defined $b) {
                print "Hata: Eksik operand.\n";
                return;
            }
            if ($token eq '+') { push @eval_stack, $a + $b }
            elsif ($token eq '-') { push @eval_stack, $a - $b }
            elsif ($token eq '*') { push @eval_stack, $a * $b }
            elsif ($token eq '/') {
                if ($b == 0) {
                    print "Sıfıra bölme hatası.\n";
                    return;
                }
                push @eval_stack, $a / $b;
            }
        }
    }
    
    return $eval_stack[0];
}

sub trim {
    my ($s) = @_;
    $s =~ s/^\s+|\s+$//g;
    return $s;
}