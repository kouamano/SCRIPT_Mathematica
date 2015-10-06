#!/usr/bin/perl

while(<>){
	push(@arr,$_);
}
$stra = join("",@arr);

$stra =~ s/\s/ /g;
$stra =~ s/\\ / /g;
$stra =~ s/ +/ /g;

#drop comment
$stra =~ s/\(\*.+?\*\)//g;

#insert LF
$stra =~ s/Cell\[/\nCell\[/g;
@arr = split(/\n/,$stra);

foreach(@arr){
	print "$_\n";
}
