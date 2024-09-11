# ELSA tool master

## Current country/project branches here:
1. Colombia
2. Colombia RAPE
3. Uganda
4. Kazakhstan
5. Peru
6. Peru Ucayali
7. Dominican Republic
8. Haiti
9. Cambodia
10. Ecuador
11. South Africa
12. Chile
13. Ecuador
14. Nepal (via UNBL interface)
15. Liberia

## To do before creating a country's tool
1. Update with country data - point `pre_global.R` to correct googlesheet and tab.
2. Update language parameter in `global.R`
3. Update country name parameter in `global.R`
4. Update `wgta_*.rds` parameter in `global.R`
5. If not run previsouly, run pre_global.R (**note that calibration may take some time...**)
	1. Set both `to_project` and `to_calib` to `TRUE`
	2. Set both back to `FALSE` after completion


## Dockerisation of the tool
Run the following


1. `docker compose up` OR `docker compose up --build` if needed to update 
2. Expose to the outside world using magic?

---

# URLs to national tools

- https://csl.gis.unbc.ca/Cambodia_ELSA/
- https://csl.gis.unbc.ca/Colombia_ELSA/
- https://csl.gis.unbc.ca/CRI_ELSA_v3/
- https://csl.gis.unbc.ca/Ecuador_ELSA/
- https://csl.gis.unbc.ca/ELSA_Tier1_CRI/
- https://csl.gis.unbc.ca/Haiti_ELSA/
- https://csl.gis.unbc.ca/Peru_ELSA/
- https://csl.gis.unbc.ca/SouthAfrica_ELSA/
- https://csl.gis.unbc.ca/Chile_ELSA/
- https://csl.gis.unbc.ca/DominicanRepublic_ELSA/
- https://csl.gis.unbc.ca/ELSA_Tier1_Colombia/
- https://csl.gis.unbc.ca/ELSA_Tier1_SouthAfrica/
- https://csl.gis.unbc.ca/Kazakhstan_ELSA/
- https://csl.gis.unbc.ca/PERU_Ucayali/
- https://csl.gis.unbc.ca/Uganda_ELSA/
- https://csl.gis.unbc.ca/Cambodia_dgis/
- https://csl.gis.unbc.ca/Cambodia_public/
- https://csl.gis.unbc.ca/Liberia_ELSA/