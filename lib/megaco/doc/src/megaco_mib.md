# Megaco mib

## Intro

The Megaco mib is as of yet not standardized and our implementation is based on *draft-ietf-megaco-mib-04.txt*. Almost all of the mib cannot easily be implemented by the megaco application. Instead these things should be implemented by a user (of the megaco application).

So what part of the mib is implemented? Basically the relevant statistic counters of the *MedGwyGatewayStatsEntry*.

## Statistics counters

The implementation of the statistic counters is lightweight. I.e. the statistic counters are handled separately by different entities of the application. For instance our two transport module(s) (see [megaco_tcp](`m:megaco_tcp#stats`) and [megaco_udp](`m:megaco_udp#stats`)) maintain their own counters and the application engine (see [megaco](`m:megaco#stats`)) maintain its own counters.

This also means that if a user implement their own transport service then it has to maintain its own statistics.

## Distribution

Each megaco application maintains its own set of counters. So in a large (distributed) MG/MGC it could be necessary to collect the statistics from several nodes (each) running the megaco application (only one of them with the transport).
