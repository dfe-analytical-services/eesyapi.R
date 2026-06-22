# Get publications

Get publications

## Usage

``` r
get_publications(
  ees_environment = NULL,
  api_version = NULL,
  search = NULL,
  page_size = 40,
  page = NULL,
  verbose = FALSE
)
```

## Arguments

- ees_environment:

  EES ees_environment to connect to: "dev", "test", "preprod" or "prod"

- api_version:

  EES API version

- search:

  String for filtering the publication list for publication titles and
  summaries containing the search string provided (strings separated by
  spaces are combined with OR logic).

- page_size:

  Number of results to return in a single query

- page:

  Page number of query results to return

- verbose:

  Run with additional contextual messaging. Logical, default = FALSE

## Value

Data frame listing all available publications

## Examples

``` r
get_publications()
#>                                      id
#> 1  fcda2962-82a6-4052-afa2-ea398c53c85f
#> 2  412d8090-ab45-455a-c176-08dbf5ab522b
#> 3  f51895df-c682-45e6-b23e-3138ddbfdaeb
#> 4  f657afb4-8f4a-427d-a683-15f11a2aefb5
#> 5  5becb18e-852b-4cdf-e2e8-08dcc3489646
#> 6  cbbd299f-8297-44bc-92ac-558bcf51f8ad
#> 7  89869bba-0c00-40f7-b7d6-e28cb904ad37
#> 8  3f3a66ec-5777-42ee-b427-8102a14ce0c5
#> 9  c8756008-ed50-4632-9b96-01b5ca002a43
#> 10 3260801d-601a-48c6-93b7-cf51680323d1
#> 11 8b7474f9-5870-4ecc-7557-08da5f64dcf1
#> 12 a91d9e05-be82-474c-85ae-4913158406d0
#> 13 657a20f6-13ef-494f-c9c0-08d82a49a1d0
#> 14 9676af6b-d563-41f4-d071-08da8f468680
#> 15 11d3385f-8d48-4e07-1255-08db886b552e
#> 16 13484f09-40e7-4b8b-7558-08da5f64dcf1
#> 17 b70e71fa-5767-4fb5-c9bf-08d82a49a1d0
#> 18 61784b00-d1e7-4dbd-c9c1-08d82a49a1d0
#> 19 2ee2b32a-3fa0-42bb-c9c2-08d82a49a1d0
#> 20 34a2c514-d603-4246-f47b-08db2568bd10
#> 21 2e510281-ca8c-41bf-bbe0-fd15fcc81aae
#> 22 a5b2d325-d8a2-4cad-f47a-08db2568bd10
#>                                                                                             title
#> 1                                                    Early years foundation stage profile results
#> 2                                                                                 Apprenticeships
#> 3  Outcomes for children in need, including children looked after by local authorities in England
#> 4                                                            Special educational needs in England
#> 5                                                              Phonics screening check attainment
#> 6                                                             Pupil absence in schools in England
#> 7                                                                                Children in need
#> 8                                                              A level and other 16 to 18 results
#> 9                                                                         Key stage 4 performance
#> 10                                           Children looked after in England including adoptions
#> 11                                                                         Key stage 2 attainment
#> 12                                                      Schools, pupils and their characteristics
#> 13                                                                     16-18 destination measures
#> 14                                                                    Pupil attendance in schools
#> 15                                                                    Further education workforce
#> 16                                                         Multiplication tables check attainment
#> 17                                                               Key stage 4 destination measures
#> 18                                                    Progression to higher education or training
#> 19                                                                       Longer term destinations
#> 20                                                                     Children missing education
#> 21                                                                              NEET age 16 to 24
#> 22                                                                        Elective home education
#>                                                                                             slug
#> 1                                                   early-years-foundation-stage-profile-results
#> 2                                                                                apprenticeships
#> 3  outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england
#> 4                                                           special-educational-needs-in-england
#> 5                                                             phonics-screening-check-attainment
#> 6                                                            pupil-absence-in-schools-in-england
#> 7                                                                               children-in-need
#> 8                                                             a-level-and-other-16-to-18-results
#> 9                                                                        key-stage-4-performance
#> 10                                          children-looked-after-in-england-including-adoptions
#> 11                                                                        key-stage-2-attainment
#> 12                                                       school-pupils-and-their-characteristics
#> 13                                                                    16-18-destination-measures
#> 14                                                                   pupil-attendance-in-schools
#> 15                                                                   further-education-workforce
#> 16                                                        multiplication-tables-check-attainment
#> 17                                                              key-stage-4-destination-measures
#> 18                                                   progression-to-higher-education-or-training
#> 19                                                                      longer-term-destinations
#> 20                                                                    children-missing-education
#> 21                                                                  neet-statistics-annual-brief
#> 22                                                                       elective-home-education
#>                                                                                                                                                             summary
#> 1             Annual statistics on early years foundation stage profile assessments in England relating to the 7 areas of learning and the 17 early learning goals.
#> 2                              Apprenticeship starts, achievements and participation. Includes breakdowns by age, sex, ethnicity, subject, provider, geography etc.
#> 3  Children in need, including children looked after by local authorities in England, national and local authority level outcomes including key stage 4 and absence
#> 4         Pupils in England with SEN support or an education, health and care (EHC) plan . Including type of need, age, sex, free school meals (FSM) and ethnicity.
#> 5                  Phonics screening check attainment statistics of pupils in England by pupil characteristics, school characteristics, region and local authority.
#> 6      Pupil absence, including overall, authorised and unauthorised absence and persistent absence by reason and pupil characteristics for the full academic year.
#> 7        Statistics on children in need in England, including child protection plans and referrals to and assessments completed by children’s social care services.
#> 8      Attainment and retention data for A level and other qualifications by age 16-18 in England. Includes region, institution type, characteristics, and subject.
#> 9  GCSE results of pupils at the end of KS4 attending state-funded schools in England at national & LA level. This release includes pupil characteristic breakdowns
#> 10     Children looked after, care leavers and children adopted in England. Annual statistics including characteristics, placement information and health outcomes.
#> 11                                                  Key stage 2 attainment statistics by pupil characteristics, school characteristics, region and local authority.
#> 12             School and pupil statistics for England including age, gender, free school meals (FSM), ethnicity, English as additional language (EAL), class size.
#> 13  Students sustaining an education, apprenticeship or employment destination after 16-18 study in England. Includes disadvantage, ethnicity and other breakdowns.
#> 14                                                                                  Fortnightly data on pupil attendance, including by reason, phase and geography.
#> 15                                               This annual official statistics release includes data on the further education workforce and governors in England.
#> 16                                             Multiplication tables check attainment by pupil characteristics, school characteristics, region and local authority.
#> 17          Pupils sustaining an education, apprenticeship or employment destination after GCSEs in England. Includes disadvantage, ethnicity and other breakdowns.
#> 18     Rates and value-added scores of level 3 (e.g., A level) students that sustain a higher education or apprenticeship destination. Includes various breakdowns.
#> 19                        Sustained education, apprenticeship or employment destinations 1, 3 and 5 years after GCSEs. Includes disadvantage and gender breakdowns.
#> 20                                                     This publication provides data reported by local authorities on children missing education (CME) in England.
#> 21                          Annual estimates from the Labour Force Survey of young people aged 16 to 24 not in education, employment or training (NEET) in England.
#> 22                                            This publication provides data reported by local authorities on children in elective home education (EHE) in England.
#>                lastPublished
#> 1  2025-11-27T09:30:14+00:00
#> 2  2026-03-26T09:30:13+00:00
#> 3  2026-04-02T08:30:06+00:00
#> 4  2026-06-11T08:30:04+00:00
#> 5  2025-10-09T08:30:04+00:00
#> 6  2026-03-26T09:30:15+00:00
#> 7  2025-10-30T09:30:08+00:00
#> 8  2026-04-23T08:30:14+00:00
#> 9  2026-04-23T08:30:12+00:00
#> 10 2025-11-26T08:32:42+00:00
#> 11 2026-04-23T08:30:10+00:00
#> 12 2026-06-04T08:30:09+00:00
#> 13 2026-04-23T08:30:07+00:00
#> 14 2026-06-11T08:30:01+00:00
#> 15 2026-05-14T08:30:03+00:00
#> 16 2025-11-20T09:30:12+00:00
#> 17 2026-04-23T08:30:13+00:00
#> 18 2026-04-23T08:30:14+00:00
#> 19 2026-05-28T08:30:07+00:00
#> 20 2026-01-15T09:30:07+00:00
#> 21 2026-03-12T09:30:04+00:00
#> 22 2026-01-15T09:30:06+00:00
get_publications(search = "attendance")
#>                                     id
#> 1 cbbd299f-8297-44bc-92ac-558bcf51f8ad
#> 2 9676af6b-d563-41f4-d071-08da8f468680
#> 3 89869bba-0c00-40f7-b7d6-e28cb904ad37
#> 4 f51895df-c682-45e6-b23e-3138ddbfdaeb
#> 5 3260801d-601a-48c6-93b7-cf51680323d1
#> 6 61784b00-d1e7-4dbd-c9c1-08d82a49a1d0
#> 7 a5b2d325-d8a2-4cad-f47a-08db2568bd10
#> 8 34a2c514-d603-4246-f47b-08db2568bd10
#>                                                                                            title
#> 1                                                            Pupil absence in schools in England
#> 2                                                                    Pupil attendance in schools
#> 3                                                                               Children in need
#> 4 Outcomes for children in need, including children looked after by local authorities in England
#> 5                                           Children looked after in England including adoptions
#> 6                                                    Progression to higher education or training
#> 7                                                                        Elective home education
#> 8                                                                     Children missing education
#>                                                                                            slug
#> 1                                                           pupil-absence-in-schools-in-england
#> 2                                                                   pupil-attendance-in-schools
#> 3                                                                              children-in-need
#> 4 outcomes-for-children-in-need-including-children-looked-after-by-local-authorities-in-england
#> 5                                          children-looked-after-in-england-including-adoptions
#> 6                                                   progression-to-higher-education-or-training
#> 7                                                                       elective-home-education
#> 8                                                                    children-missing-education
#>                                                                                                                                                            summary
#> 1     Pupil absence, including overall, authorised and unauthorised absence and persistent absence by reason and pupil characteristics for the full academic year.
#> 2                                                                                  Fortnightly data on pupil attendance, including by reason, phase and geography.
#> 3       Statistics on children in need in England, including child protection plans and referrals to and assessments completed by children’s social care services.
#> 4 Children in need, including children looked after by local authorities in England, national and local authority level outcomes including key stage 4 and absence
#> 5     Children looked after, care leavers and children adopted in England. Annual statistics including characteristics, placement information and health outcomes.
#> 6     Rates and value-added scores of level 3 (e.g., A level) students that sustain a higher education or apprenticeship destination. Includes various breakdowns.
#> 7                                            This publication provides data reported by local authorities on children in elective home education (EHE) in England.
#> 8                                                     This publication provides data reported by local authorities on children missing education (CME) in England.
#>               lastPublished
#> 1 2026-03-26T09:30:15+00:00
#> 2 2026-06-11T08:30:01+00:00
#> 3 2025-10-30T09:30:08+00:00
#> 4 2026-04-02T08:30:06+00:00
#> 5 2025-11-26T08:32:42+00:00
#> 6 2026-04-23T08:30:14+00:00
#> 7 2026-01-15T09:30:06+00:00
#> 8 2026-01-15T09:30:07+00:00
```
