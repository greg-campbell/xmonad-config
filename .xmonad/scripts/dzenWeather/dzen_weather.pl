#!/usr/bin/perl
#
# (c) 2007 by Robert Manea

use Weather::Com::Finder;

# Fill with valid values
#
my $PartnerId  = '1183495725';
my $LicenseKey = '1616af0eb77bbd26';
my $iconpath = '/home/gregcamp/.icons/weather';

my %weatherargs = (
    'partner_id' => $PartnerId,
    'license'    => $LicenseKey,
    'units'      => 's',
);

my $weather_finder = Weather::Com::Finder->new(%weatherargs);

###################################### Fill in your location
my $locations = $weather_finder->find('Seattle, Washington, USA');

my $temp_today =
    $locations->[0]->current_conditions()->temperature();
my $desc_today = $locations->[0]->current_conditions()->icon();

my $forecast = $locations->[0]->forecast();
my $temp_tomorrow =
    $forecast->day(1)->high();
my $desc_tomorrow = $forecast->day(1)->day->icon();
    $forecast->day(1)->day->conditions;
my $temp_dat =
    $forecast->day(2)->high();
my $desc_dat = $forecast->day(2)->day->icon();

print "$temp_today", "\n";
print "test";
