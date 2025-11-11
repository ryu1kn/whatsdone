# Plan 001: LLM Eval Setup for "Done" Topic Categorization

## 1. Goal

The primary goal is to create a robust evaluation framework for an LLM-based topic categorization system for "dones". This framework will enable confident development, prevent regressions, and ensure the quality of the categorization. The guiding principle for this plan is simplicity.

## 2. High-Level Approach

We will build a command-line tool within the `tools/llm-eval` directory. This tool will evaluate the performance of the LLM-based topic categorization by comparing its output against a "ground truth" dataset.

The process will be:
1.  Create a dataset of "dones" with manually assigned topics (the ground truth).
2.  Use an LLM to categorize the "dones" from the dataset.
3.  Compare the LLM's predicted topics with the ground truth topics.
4.  Generate a report with performance metrics.

## 3. Proposed Directory Structure

The `tools/llm-eval` directory will be structured as follows to keep things organized:

```
tools/llm-eval/
├── data/
│   ├── dataset.jsonl  # Evaluation dataset ("done" text and ground truth topic)
│   └── topics.json    # List of allowed topics
├── src/
│   ├── __main__.py    # Main entry point for the evaluation tool
│   ├── categorizer.py # Logic for interacting with the LLM
│   └── metrics.py     # Logic for computing evaluation metrics
├── reports/           # Directory to store evaluation reports
├── pyproject.toml     # Python project configuration and dependencies
└── README.md          # Instructions on how to use the tool
```

## 4. Components

### 4.1. Evaluation Dataset (`data/dataset.jsonl`)

-   A collection of "done" items stored in JSONL format (one JSON object per line).
    - **Note**: The source for this file should be `tools/done-analysis/data-local/done-dump--20250504-2251.json`. You will need to process this JSON array into a JSONL format, extracting the `doneThing` field as the `text` and adding an `expected_topic` field (initially, this can be a placeholder or derived using a simple heuristic).
-   Each object will have two keys: `text` (the content of the "done") and `expected_topic` (the ground truth topic).
-   This dataset will be manually created initially and version-controlled in Git.

### 4.2. Topic Schema (`data/topics.json`)

-   A JSON file containing a list of topic objects, each with a `name` and a `description`.
-   The `name` field represents the topic, and the `description` provides clarifying information on when to use each topic.
-   This ensures that the categorization is consistent and constrained to a predefined set of topics, with clear guidance for categorization.

### 4.3. Categorizer (`src/categorizer.py`)

-   This module will contain the logic to take a "done" text as input and use an LLM to assign a topic.
-   It will include an abstraction for the LLM API, making it easy to switch between different LLM providers or models in the future.

### 4.4. Evaluation Runner (`src/__main__.py`)

-   This will be the main script to run the evaluation.
-   It will load the dataset and the topic schema.
-   It will iterate through the dataset, calling the `Categorizer` for each "done".
-   It will collect the predictions and pass them to the `Metrics` module.

### 4.5. Metrics Calculator (`src/metrics.py`)

-   This module will calculate and display performance metrics.
-   Initially, we will focus on simple **accuracy**.
-   Later, we can add more detailed metrics like **precision, recall, F1-score per topic**, and a **confusion matrix**.

## 5. Workflow

1.  **Data Preparation:** A developer will populate `data/dataset.jsonl` with a representative set of "dones" and their correct topics. `data/topics.json` will also be defined.
2.  **Run Evaluation:** The developer will run the evaluation tool from the command line: `python -m src`.
3.  **Processing:** The tool will process each "done" in the dataset, get the predicted topic from the LLM, and compare it to the ground truth.
4.  **Reporting:** The tool will output a summary report to the console. The report will show the key performance metrics. Detailed reports can be saved to the `reports/` directory.

## 6. Technology Choices

-   **Language:** Python will be used for this tool, given its strong ecosystem for data science and LLM-related tasks (e.g., libraries like `pandas`, `scikit-learn`, and various LLM clients).
-   **Dependencies:** Python dependencies will be managed using `pyproject.toml` and `pip`.

## 7. Next Steps

1.  Create the directory structure as outlined above.
2.  Create an initial small version of `dataset.jsonl` and `topics.json`. We will need to source some "done" examples for this.
3.  Implement the first version of the evaluation script, focusing on the core workflow and the accuracy metric.
4.  Choose and integrate an initial LLM for categorization.

This plan provides a simple yet effective foundation for building the LLM evaluation guardrails. We can start small and incrementally add more features and sophistication as needed.
