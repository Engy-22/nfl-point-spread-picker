# NFL Point Spread Picker

This project recommends teams to pick in NFL point spread on Ontario Lottery and Gaming (OLG). Odds are taken from [OLG](https://www.proline.ca/#pointspread) and [Oddshark](http://www.oddsshark.com/nfl/odds).

## Instructions

1. Go to https://www.proline.ca/#pointspread and roughly copy all the games and paste them into a text file titled `odds-<YYYY-MM-DD>.txt` (with `<YYYY-MM-DD>` replaced with the current date) and place the file in the folder `olg-point-spreads`
2. In `nfl-point-spread-picker.R`, update `file_date` in `clean_olg_data()` with the current date
3. Run `nfl-point-spread-picker.R` and view the printed output for top games to bet on
