---
layout: post
title: "Storing Client specific data"
description: ""
categories: [misc]
tags: []
thumbnail:
---

## Context

Storing client-specific data is common when requirements vary by market, country, or tenant. Example: Customs software where each country adds unique fields to a Declaration or Manifest (e.g., EU EORI, US HTS codes, VN-specific attachments).

## Design goals

- Flexibility: adapt per country without constant schema churn.
- Validity: enforce structure and constraints.
- Queryability: support reporting, search, and compliance checks.
- Performance: index hot paths.
- Maintainability: predictable code paths, testable contracts.
- Auditability: track changes per country-specific field.

## Option 1: Store as a delimited string

Keep client- or country-specific data in a single delimited string column (e.g., comma- or pipe-separated).

Pros:

- Very simple schema; works with legacy systems.
- Easy to append new values without migrations.

Cons:

- Hard to validate and enforce structure.
- Parsing logic is error-prone (escaping, ordering, missing values).
- Poor queryability; usually requires full scans or application-side parsing.

Example (simple comma-separated list):

```text
CO-12345,perishable,expedite_clearance
```

Example (key–value pairs in a single string):

```text
country=VN,certificate_of_origin=CO-12345,special_handling=perishable|expedite_clearance
```

Use when:

- Data is low-volume and rarely queried individually.
- You are integrating with a legacy schema and plan to evolve to JSON or columns later.
- You primarily display the data back to users rather than compute on it.

## Option 2: Store in a JSON column

Keep country-specific fields in a JSON/JSONB column with lightweight validation and indexes.

Pros:

- Flexible schema; easy to evolve.
- Queryable with JSON functions.
- Good for partial adoption per country.

Cons:

- Validation is weaker than strict columns unless augmented.
- Type safety and tooling vary.

PostgreSQL setup:

```sql
-- Add JSONB column with default and basic type check
ALTER TABLE declarations
  ADD COLUMN country_data jsonb NOT NULL DEFAULT '{}'::jsonb;

ALTER TABLE declarations
  ADD CONSTRAINT country_data_is_object
  CHECK (jsonb_typeof(country_data) = 'object');

-- Example GIN index for common queries
CREATE INDEX declarations_country_data_gin
  ON declarations USING GIN (country_data jsonb_path_ops);

-- Query: VN declarations with a required attachment
SELECT id
FROM declarations
WHERE country_data->>'country' = 'VN'
  AND (country_data->'attachments' ? 'certificate_of_origin');
```

MySQL tip:

- Use JSON and generated columns for indexing specific paths.

```sql
ALTER TABLE declarations
  ADD COLUMN country_data JSON NOT NULL,
  ADD COLUMN vn_co VARCHAR(64) GENERATED ALWAYS AS (JSON_UNQUOTE(JSON_EXTRACT(country_data, '$.vn.certificate_of_origin'))) VIRTUAL,
  ADD INDEX idx_vn_co (vn_co);
```

Use when:

- Fields vary widely across countries.
- You need moderate queryability without rigid schemas.

## Option 3: Add extra columns to the table

Promote stable, widely-used fields into first-class columns.

Pros:

- Strong validation and indexing.
- Clear contracts for core workflows.

Cons:

- Schema churn; overgrowth risk.
- Cross-country columns can become sparse.

Example:

```sql
ALTER TABLE declarations
  ADD COLUMN eu_eori VARCHAR(32),
  ADD COLUMN us_hts_code VARCHAR(16),
  ADD COLUMN vn_manifest_ref VARCHAR(64);

CREATE INDEX idx_declarations_eu_eori ON declarations (eu_eori);
```

Use when:

- A field is stable, high-signal, and frequently queried/reportable.

## Choosing an approach

- Start with JSON for flexibility.
- Promote to columns when a field stabilizes and is query-critical.
- Keep formatted text for narrative and edge notes.

Quick guide:

- Narrative only → formatted text.
- Variable structure, moderate querying → JSON column.
- Stable, critical, high-volume queries → extra columns.

## Evolution and migration

- Define JSON schema per country (OpenAPI/JSON Schema) and validate in application layer.
- Track usage; promote hot fields to columns; backfill from JSON.
- Maintain dual-write temporarily; deprecate JSON paths after migration.
- Version country_data (e.g., country_data.version) for safe changes.

## Operational considerations

- Validation: server-side schema checks per country; CI tests for contracts.
- Indexing: GIN/functional indexes on hot JSON paths; btree on promoted columns.
- Auditing: store change logs per field; consider immutable history tables.
- Performance: cap JSON nesting; avoid massive blobs; paginate attachments.
- API design: expose country-specific sections under /countries/{code}; document required fields per country.

## Customs examples (lightweight)

JSON shape:

```json
{
  "country": "VN",
  "attachments": {
    "certificate_of_origin": "CO-12345"
  },
  "declaration": {
    "hs_code": "090121",
    "special_handling": ["perishable", "expedite"]
  }
}
```

EU (promoted column example):

```sql
-- eu_eori in main table
UPDATE declarations
SET eu_eori = country_data->>'eu_eori'
WHERE country_data ? 'eu_eori';
```

<!-- TODO: add diagrams for data flow, index strategy, and migration steps -->
