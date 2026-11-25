# Static prototype

A serverless version of the Shiny app, built with Quarto and Observable JS.
Everything runs in the browser:

- **Address lookup:** the browser calls Esri's geocoder (OpenStreetMap's
  Nominatim as a backup), then finds the House district with Turf.js against
  a simplified GeoJSON of the district boundaries.
- **Results:** read from JSON files exported from the app's data.

The Shiny app in the repository root is unchanged.

## Rebuild

From the repository root, re-export the data whenever `data/` changes:

```
Rscript static/prep_static_data.R
```

Then render from this folder (Quarto ships with RStudio and Positron):

```
quarto render
```

The site is written to `_site/`. Preview it with any static file server,
e.g. `python -m http.server --directory static/_site`. Opening `index.html`
straight from disk won't work, because browsers block its data requests.

## Deploy

Upload `_site/` to any static host (Netlify, GitHub Pages, etc.). No server
or R is needed at runtime. URL parameters work like the Shiny app's:
`?district=45&party=Dem` or `?address=1007 E Grand Ave Des Moines&party=Rep`.

## Notes

- The boundaries are simplified with `sf::st_simplify` (10 m tolerance) to
  ~0.5 MB. They agree with the full shapefile on 99.99% of 50,000 random
  points. A point that falls in a sliver between districts goes to the
  nearest one within 50 m.
- City-only and ZIP-only searches are rejected, since their center could be
  in any district.
