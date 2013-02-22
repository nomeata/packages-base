#!/usr/bin/perl

use File::Slurp;
use File::Find::Rule;

print <<"";
digraph base {
	compound = true; 
//	clusterrank = none;
        ranksep = 2;
        mclimit = 100;
        nslimit = 100;
        rankdir = LR;

@colors = qw/aquamarine chartreuse1 darkorange2 coral gold1 darksalmon darkslategray2 cyan3 firebrick1 skyblue1/;

my @packages = qw/base-pure base-st base-io base-array base-float base-concurrent base-foreign base-io-system/;

printf <<'', pop @colors;
subgraph "cluster_ghc-prim" {
label = "ghc-prim";
color = "black";
node [style=filled,fillcolor=%s];
"GHC.Prim";
"GHC.Classes";
"GHC.CString";
//"GHC.Debug";
"GHC.Magic";
//"GHC.PrimopWrappers";
//"GHC.IntWord64";
"GHC.Tuple";
"GHC.Types";
}

printf <<'', pop @colors;
subgraph "cluster_integer-gmp" { label = "integer-gmp"; color = "black";
node [style=filled,fillcolor=%s];
"GHC.Integer";
"GHC.Integer.Logarithms";
"GHC.Integer.Logarithms.Internals";
"GHC.Integer.GMP.Internals";
}

my %seen;
my %all;

for my $pkg (@packages) {
	printf <<'', $pkg, $pkg, pop @colors;
	subgraph "cluster_%s" {
		label = "%s";
		color = "black";
		node [style=filled,fillcolor=%s];

	my $rule = File::Find::Rule->file()->name("*.hs", "*.lhs")->start($pkg);
	OUT: while (defined (my $file = $rule->match)) {
		for (read_file($file)) {
			if (m!module ([A-Z][a-zA-Z0-9\.]*)!) {
				$seen{$1} = $pkg;
				printf "\"%s\";\n", $1;
			}
		}
	}
	printf "}\n";
}


for $file (<*.imports>) {
	my ($from) = ($file =~ m!^(.*).imports!);
	$all{$from}++;
	for (read_file($file)) {
		if (m!import (?:safe )?(?:qualified )?(?:{-# SOURCE #-} )?([A-Z][a-zA-Z0-9\.]*)!) {
			$all{$1}++;
			printf "\"%s\" -> \"%s\"", $from, $1;
			#if ($seen{$1} && !($seen{$1} eq $seen{$from})) {
			#	printf " [lhead=\"cluster_%s\"]", $seen{$1};
			#}
			printf ";\n";
		}
	}
}

printf "subgraph \"cluster_unsorted\" { label = \"unsorted\"; color = black; \n", $pkg, $pkg;
for $mod (keys %all) {
	next if $seen{$mod};
	printf "\"%s\";\n",$mod;
}
printf "}\n";

print "}\n"
