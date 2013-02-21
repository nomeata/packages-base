#!/usr/bin/perl

use strict;
use warnings;
use File::Find::Rule;

my @packages = ("base-pure", "base-st", "base-io", "base-concurrent", "base-foreign");

my %files;
#$files{$_} = {} for @packages;
#$files{base} = {};

my $rule = File::Find::Rule->file()->start(".");
OUT: while (defined (my $file = $rule->match)) {
	$file =~ s!^./!!;
	next if $file =~ m!^tests/!;
	next if $file =~ m!^\.!;
	next if $file =~ m!^dist!;
	for my $pkg (@packages) {
		if ($file =~ m!^$pkg/(.*)!) {
			my $file = $1;
			next OUT if $file =~ m!^dist!;
			$files{$file} ||= [];
			push @{$files{$file}}, $pkg;
			next OUT;
		}
	}
	$files{$file} ||= [];
	push @{$files{$file}}, 'base';
}

for my $file (sort keys %files) {
	if (scalar @{$files{$file}} < 2) {
		printf "%s only in package %s\n", $file, $files{$file}[0];
	}
	if (scalar @{$files{$file}} > 2) {
		printf "%s in multiple packages: %s\n", $file, join (", ",@{$files{$file}});
	}
}
