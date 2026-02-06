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
#> 3  5becb18e-852b-4cdf-e2e8-08dcc3489646
#> 4  89869bba-0c00-40f7-b7d6-e28cb904ad37
#> 5  c8756008-ed50-4632-9b96-01b5ca002a43
#> 6  8b7474f9-5870-4ecc-7557-08da5f64dcf1
#> 7  3260801d-601a-48c6-93b7-cf51680323d1
#> 8  13484f09-40e7-4b8b-7558-08da5f64dcf1
#> 9  9676af6b-d563-41f4-d071-08da8f468680
#> 10 34a2c514-d603-4246-f47b-08db2568bd10
#> 11 a5b2d325-d8a2-4cad-f47a-08db2568bd10
#>                                                   title
#> 1          Early years foundation stage profile results
#> 2                                       Apprenticeships
#> 3                    Phonics screening check attainment
#> 4                                      Children in need
#> 5                               Key stage 4 performance
#> 6                                Key stage 2 attainment
#> 7  Children looked after in England including adoptions
#> 8                Multiplication tables check attainment
#> 9                           Pupil attendance in schools
#> 10                           Children missing education
#> 11                              Elective home education
#>                                                    slug
#> 1          early-years-foundation-stage-profile-results
#> 2                                       apprenticeships
#> 3                    phonics-screening-check-attainment
#> 4                                      children-in-need
#> 5                               key-stage-4-performance
#> 6                                key-stage-2-attainment
#> 7  children-looked-after-in-england-including-adoptions
#> 8                multiplication-tables-check-attainment
#> 9                           pupil-attendance-in-schools
#> 10                           children-missing-education
#> 11                              elective-home-education
#>                                                                                                                                                             summary
#> 1             Annual statistics on early years foundation stage profile assessments in England relating to the 7 areas of learning and the 17 early learning goals.
#> 2                              Apprenticeship starts, achievements and participation. Includes breakdowns by age, sex, ethnicity, subject, provider, geography etc.
#> 3                  Phonics screening check attainment statistics of pupils in England by pupil characteristics, school characteristics, region and local authority.
#> 4        Statistics on children in need in England, including child protection plans and referrals to and assessments completed by children’s social care services.
#> 5  GCSE results of pupils at the end of KS4 attending state-funded schools in England at national & LA level. This release includes pupil characteristic breakdowns
#> 6                                                   Key stage 2 attainment statistics by pupil characteristics, school characteristics, region and local authority.
#> 7      Children looked after, care leavers and children adopted in England. Annual statistics including characteristics, placement information and health outcomes.
#> 8                                              Multiplication tables check attainment by pupil characteristics, school characteristics, region and local authority.
#> 9       Pupil attendance and absence data including termly national statistics and fortnightly statistics in development derived from DfE’s regular attendance data
#> 10                                                     This publication provides data reported by local authorities on children missing education (CME) in England.
#> 11                                            This publication provides data reported by local authorities on children in elective home education (EHE) in England.
#>                lastPublished
#> 1  2025-11-27T09:30:14+00:00
#> 2  2026-01-29T09:30:26+00:00
#> 3  2025-10-09T08:30:04+00:00
#> 4  2025-10-30T09:30:08+00:00
#> 5  2025-10-16T08:30:08+00:00
#> 6  2025-12-11T09:30:07+00:00
#> 7  2025-11-26T08:32:42+00:00
#> 8  2025-11-20T09:30:12+00:00
#> 9  2026-02-05T09:30:07+00:00
#> 10 2026-01-15T09:30:07+00:00
#> 11 2026-01-15T09:30:06+00:00
get_publications(search = "attendance")
#>                                     id
#> 1 9676af6b-d563-41f4-d071-08da8f468680
#> 2 89869bba-0c00-40f7-b7d6-e28cb904ad37
#> 3 3260801d-601a-48c6-93b7-cf51680323d1
#> 4 a5b2d325-d8a2-4cad-f47a-08db2568bd10
#> 5 34a2c514-d603-4246-f47b-08db2568bd10
#>                                                  title
#> 1                          Pupil attendance in schools
#> 2                                     Children in need
#> 3 Children looked after in England including adoptions
#> 4                              Elective home education
#> 5                           Children missing education
#>                                                   slug
#> 1                          pupil-attendance-in-schools
#> 2                                     children-in-need
#> 3 children-looked-after-in-england-including-adoptions
#> 4                              elective-home-education
#> 5                           children-missing-education
#>                                                                                                                                                        summary
#> 1  Pupil attendance and absence data including termly national statistics and fortnightly statistics in development derived from DfE’s regular attendance data
#> 2   Statistics on children in need in England, including child protection plans and referrals to and assessments completed by children’s social care services.
#> 3 Children looked after, care leavers and children adopted in England. Annual statistics including characteristics, placement information and health outcomes.
#> 4                                        This publication provides data reported by local authorities on children in elective home education (EHE) in England.
#> 5                                                 This publication provides data reported by local authorities on children missing education (CME) in England.
#>               lastPublished
#> 1 2026-02-05T09:30:07+00:00
#> 2 2025-10-30T09:30:08+00:00
#> 3 2025-11-26T08:32:42+00:00
#> 4 2026-01-15T09:30:06+00:00
#> 5 2026-01-15T09:30:07+00:00
```
