# Basic tidymatrix usage examples
library(tidymatrix)
library(dplyr)

# Example 1: Questionnaire data
# Matrix: survey responses (people x questions)
# Row metadata: demographics
# Column metadata: question information

# Create sample data
set.seed(123)
n_people <- 100
n_questions <- 10

# Survey response matrix (Likert scale 1-5)
responses <- matrix(
  sample(1:5, n_people * n_questions, replace = TRUE),
  nrow = n_people,
  ncol = n_questions
)

# Row metadata: people demographics
people_data <- data.frame(
  person_id = 1:n_people,
  age = sample(18:80, n_people, replace = TRUE),
  gender = sample(c("M", "F", "Other"), n_people, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Master", "PhD"),
                     n_people, replace = TRUE)
)

# Column metadata: question information
question_data <- data.frame(
  question_id = 1:n_questions,
  category = rep(c("Demographics", "Satisfaction", "Behavior"),
                 length.out = n_questions),
  question_text = paste("Question", 1:n_questions),
  required = sample(c(TRUE, FALSE), n_questions, replace = TRUE)
)

# Create tidymatrix object
tm <- tidymatrix(responses, people_data, question_data)
print(tm)

# Example 2: Working with rows (people)
# Filter to only people over 30
tm_filtered <- tm %>%
  activate(rows) %>%
  filter(age > 30)

print(tm_filtered)

# Add a new column: age group
tm_with_groups <- tm %>%
  activate(rows) %>%
  mutate(age_group = case_when(
    age < 30 ~ "Young",
    age < 50 ~ "Middle",
    TRUE ~ "Senior"
  ))

print(tm_with_groups)

# Select specific metadata columns
tm_selected <- tm %>%
  activate(rows) %>%
  select(person_id, age, gender)

print(tm_selected)

# Example 3: Working with columns (questions)
# Filter to only required questions
tm_required <- tm %>%
  activate(columns) %>%
  filter(required == TRUE)

print(tm_required)

# Add question numbers
tm_numbered <- tm %>%
  activate(columns) %>%
  mutate(q_number = paste0("Q", question_id))

print(tm_numbered)

# Example 4: Chaining operations
# Get satisfaction questions for people over 40
tm_complex <- tm %>%
  activate(rows) %>%
  filter(age > 40, gender == "F") %>%
  activate(columns) %>%
  filter(category == "Satisfaction") %>%
  activate(rows) %>%
  arrange(age)

print(tm_complex)

# Example 5: Gene expression data
# Matrix: expression values (genes x samples)
# Row metadata: gene annotations
# Column metadata: sample information

n_genes <- 50
n_samples <- 20

# Expression matrix
expression <- matrix(
  rnorm(n_genes * n_samples, mean = 5, sd = 2),
  nrow = n_genes,
  ncol = n_samples
)

# Gene annotations
gene_data <- data.frame(
  gene_id = paste0("GENE", 1:n_genes),
  gene_symbol = paste0("SYM", 1:n_genes),
  chromosome = sample(1:22, n_genes, replace = TRUE),
  pathway = sample(c("Metabolism", "Signaling", "Transport"),
                   n_genes, replace = TRUE)
)

# Sample information
sample_data <- data.frame(
  sample_id = paste0("S", 1:n_samples),
  condition = rep(c("Control", "Treatment"), each = n_samples/2),
  batch = rep(1:4, each = n_samples/4),
  patient_id = rep(1:10, each = 2)
)

# Create gene expression tidymatrix
expr_tm <- tidymatrix(expression, gene_data, sample_data)

# Filter to metabolism genes and treatment samples
expr_filtered <- expr_tm %>%
  activate(rows) %>%
  filter(pathway == "Metabolism") %>%
  activate(columns) %>%
  filter(condition == "Treatment")

print(expr_filtered)
