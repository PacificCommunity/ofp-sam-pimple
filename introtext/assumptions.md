## Key assumptions

### Harvest control rule output

The fisheries in the evaluations are either managed through catch or effort limits, set by the HCR output.

The HCR output is a multiplier applied to the average catch or effort in the baseline period, depending on the fishery.

| Fishery | Catch or effort managed &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;| HCR baseline period |
|:------------------|:--------------:|---------:|
| Purse seine | Effort | 2012 |
| Pole and line | Effort | 2001-2004 |
| ID, VN and PH fisheries (excl. ID and PH purse seine) | Catch | 2016-2018 |
| ID and PH purse seine fishery | Effort | 2012 |


### Timing of the management procedure

The management period is three years.

The management proedure uses data up to previous year to set the catch and effort limits in the following year.
For example, the management procedure is first run in 2022, using data up to 2021, to set the fishing levels for 2023-2025.
It is next run in 2025, using data up to 2024, to set the fishing levels for 2026-2028; and so on.

### Transient period

The last data year in the operating models is 2018.
The time between the last data year and the fishing level being set by the management procedure in 2023 is known as the *transient period*, i.e. 2019 to 2022.
In the evaluations the level of fishing in the transient period is set to the average of 2016-2018.

### Harvest control rule constraints

An HCR can have an additional constraint, e.g. the new HCR output cannot change by more than 10% from the previous HCR output.
When the management procedure is first run (at the end of the transient period) the constraint is applied to a value of 1.
For example, if there is a +- 10% constraint, the first time the management procedure is called the output of the HCR cannot be more than 1.1 or less than 0.9.