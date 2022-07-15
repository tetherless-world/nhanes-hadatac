# NHANES-HADatAc

Welcome to the NHANES-HADatAc Github. This repository has the objective of storing all HADatAc artifacts developed in support of NHANES. In here, you will find the complete description of the contents, and how to use them. This README is being constantly updated.

**If you are a reviewer** of our poster submission to ISWC 2022, the table below summarizes the mentioned resources in the paper.

| Resource | Filename |   |
| -------- | -------- | - |
| NHANES Drug Ontology - Drugs | `rxq_drug.ttl` | [Link](https://github.com/tetherless-world/nhanes-hadatac/blob/main/ontologies/rxq_drug.ttl) |
| NHANES Drug Ontology - Therapeutic classification | `mltcs.ttl` | [Link](https://github.com/tetherless-world/nhanes-hadatac/blob/main/ontologies/mltcs.ttl) |
| Prescription Medications (RXQ_RX) Prevention SDD | `SDD-NHANES-RXQ_RX-P.xlsx` | [Link](https://github.com/tetherless-world/nhanes-hadatac/blob/main/metadata/SDD-NHANES-RXQ_RX-P.xlsx) |
| Prescription Medications (RXQ_RX) Treatment SDD | `SDD-NHANES-RXQ_RX-T.xlsx` | [Link](https://github.com/tetherless-world/nhanes-hadatac/blob/main/metadata/SDD-NHANES-RXQ_RX-T.xlsx) |
| SETLr scripts | `rxq_drug.setl.ttl` & `mltcs.setl.ttl` | [Link](https://github.com/tetherless-world/nhanes-hadatac/tree/main/setlr)

## Supporting Ontologies

Location: `/ontologies`

| Ontology | Filename |
| -------- | -------- |
| NHANES Drug Ontology - Drugs | `rxq_drug.ttl` |
| NHANES Drug Ontology - Therapeutic classifiction | `mltcs.ttl` |
| NHANES Ontology | `nhanes.ttl` |

## HADatAc Metadata

Location: `/metadata`

| Type | Filename |
| ---- | -------- |
| Study files - STD | `STD-NHANES-*.csv` |
| Semantic Study Designs - SSD | `SSD-NHANES-*.xlsx` |
| Semantic Data Dictionaries - SDD | `SDD-NHANES-*.xlsx` |
| Deploynents - DPL | `DPL-NHANES.xlsx` |
| Data Acquisitions - STR | `STR-NHANES.xlsx` |

### Available Semantic Data Dictionaries

| NHANES dataset | Type | Filename |
| -------------- | ---- | -------- |
| Demographic Variables and Sample Weights (DEMO) | Demographics | `SDD-NHANES-DEMO.xlsx` |
| Cholesterol - Total (TCHOL) | Laboratory | `SDD-NHANES-LAB-RESULTS.xlsx` |
| Glycohemoglobin (GHB) | Laboratory | `SDD-NHANES-LAB-RESULTS.xlsx` |
| Plasma Fasting Glucose (GLU) | Laboratory | `SDD-NHANES-LAB-RESULTS.xlsx` |
| Urine Pregnancy Test (UCPREG) | Laboratory | `SDD-NHANES-LAB-RESULTS.xlsx` |
| Blood Pressure & Cholesterol (BPQ) | Questionnaire | `SDD-NHANES-BPQ.xlsx` |
| Diabetes (DIQ) | Questionnaire | `SDD-NHANES-DIQ.xlsx` |
| Health Insurance (HIQ) | Questionnaire | `SDD-NHANES-HIQ.xlsx` |
| Prescription Medications (RXQ_RX) | Questionnaire | `SDD-NHANES-RXQ_RX-*.xlsx` |
