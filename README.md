# Project Proposal: Stock Market Interactive Visualizer

**COMP4010/5120: Data Visualization - Spring 2025**  
---

## 1. High-Level Goal

Develop an interactive R Shiny web application to visualize historical stock market data by company and time range. The app will include dynamic charts for price trends, trading volume, and volatility, allowing users to explore market behavior across different stocks using real data.

---

## 2. Goals and Motivation

The stock market is a dynamic system where prices and trading volume constantly fluctuate based on investor behavior, company performance, and global events. However, most raw stock datasets are hard to interpret without proper tools. Our goal is to make stock data exploration simple, intuitive, and interactive.

We aim to build a Shiny dashboard that allows users to:
- Select specific stocks to explore
- Visualize **price trends over time** (line charts with `plotly`)
- Compare **daily or monthly trading volume** (bar or heatmaps)
- Explore **volatility trends** (using calculated standard deviation of prices)
- Optionally animate stock growth over time (`gganimate`)

This project gives us the opportunity to apply skills beyond the course material, including working with time series data, implementing a dynamic web dashboard, and integrating advanced visualizations. It also provides a chance to practice `dplyr`, `plotly`, `ggplot2`, and `gganimate`, as well as improve UI/UX in Shiny apps.

---

## 3. Project Focus and Data Handling

### Focus

We will focus on developing a clean, responsive Shiny app that includes multiple views and filters for exploring historical stock data (Open, Close, High, Low prices, and Volume) for different companies.

### Data

- **Source:** [Kaggle – Stock Market Data](https://www.kaggle.com/datasets/marianadeem755/stock-market-data)
- **Format:** CSV format with fields: `Date`, `Open`, `High`, `Low`, `Close`, `Volume`, `Name`
- **Time Range:** Historical daily data from various companies

### Data Handling Plan

- Load and preprocess data using `readr`, `dplyr`, and `lubridate`
- Handle missing values and standardize formats
- Create additional derived columns (e.g., Daily Volatility, Month-Year)
- Group and summarize volume data for heatmaps
- Prepare aggregated `.rds` files for efficient app loading

---

## 4. Project Timeline and Milestones

| Phase | Deadline | Focus | Key Tasks |
|-------|----------|-------|-----------------------------|
| **Phase 1: Data Preparation & Setup** | May 4th | Data Acquisition & Cleaning | Download Kaggle dataset; Clean and format data; Create exploratory plots; Set up GitHub structure|
| **Phase 2: Prototype Development** | May 15th | Build Interactive Features | Build Shiny UI layout; Implement reactive filters; Create line plots for closing price; Volume and volatility chart modules|
| **Phase 3: Finalization** | May 30th | Testing, Documentation, Polish | Refactor app code; Finalize visuals with `plotly` and `ggplot2` ; Create animated growth chart; Write final report and presentation slides|

---

