{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipStartup";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Various analytics to assist in the analysis of the performance of startups.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    flipTime
    flipStandardCharts
    flipU
    scales
    lubridate
    reshape2
    verbs
    plotly
    flipFormat
    flipStatistics
    ggplot2
  ];
}
