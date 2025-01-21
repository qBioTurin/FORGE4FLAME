import pandas as pd
import os
import argparse
from plotnine import *
from glob import glob

def plot_percentage_difference_at_day_x(
    exp_dirs, baseline_exp, day_x, experiment_labels=None, output_file_prefix="percentage_difference_at_day_x"
):
    """
    Generate a bar plot showing the percentage difference in the sum of
    Exposed, Infected, and Recovered states at a specific day (day x) between a baseline scenario and other scenarios.

    Args:
        exp_dirs (list of str): List of experiment directories containing seed* subdirectories.
        baseline_exp (str): Directory of the baseline experiment.
        day_x (int): The specific day to compare (e.g., day 50).
        experiment_labels (dict): Mapping of experiment directories to custom labels for the x-axis.
        output_file_prefix (str): Prefix for the plot file name.

    Returns:
        None
    """
    data_frames = []

    for exp_dir in exp_dirs:
        experiment_name = os.path.basename(exp_dir.rstrip("/"))
        seed_dirs = glob(os.path.join("../results/" + exp_dir, "seed*"))

        for seed_dir in seed_dirs:
            file_path = os.path.join(seed_dir, "evolution.csv")
            if os.path.exists(file_path):
                df = pd.read_csv(file_path)
                df['Seed'] = os.path.basename(seed_dir)  # Add Seed info
                df['Experiment'] = experiment_name       # Add Experiment info
                data_frames.append(df)

    # Combine all data into a single DataFrame
    combined_df = pd.concat(data_frames, ignore_index=True)

    # Filter data to include only day_x
    day_x_df = combined_df[combined_df['Day'] == day_x]

    # Calculate the total sum of Exposed, Infected, and Recovered for each experiment at day_x
    day_x_df['Sum_EIR'] = day_x_df[['Exposed', 'Infected', 'Recovered']].sum(axis=1)

    # Average the sums across seeds for each experiment at day_x
    avg_day_x_df = day_x_df.groupby(['Experiment'], as_index=False)['Sum_EIR'].mean()

    # Extract baseline data
    baseline_sum = avg_day_x_df[avg_day_x_df['Experiment'] == baseline_exp]['Sum_EIR'].values[0]

    # Calculate the percentage difference relative to the baseline for other experiments
    avg_day_x_df['Percentage_Diff'] = 100 * (avg_day_x_df['Sum_EIR'] - baseline_sum) / baseline_sum

    # Filter out the baseline experiment
    plot_df = avg_day_x_df.loc[avg_day_x_df['Experiment'] != baseline_exp]

    # Map experiment names to custom labels if provided
    if experiment_labels:
        plot_df['Experiment_Label'] = plot_df['Experiment'].map(experiment_labels)
    else:
        plot_df['Experiment_Label'] = plot_df['Experiment']

    # Extract group (mask type) from experiment labels
    if experiment_labels:
        plot_df['Group'] = plot_df['Experiment_Label'].apply(lambda x: x.split('-')[0])
    else:
        plot_df['Group'] = plot_df['Experiment']

    # Sort the DataFrame by 'Percentage_Diff' (from min to max)
    plot_df = plot_df.sort_values(by='Percentage_Diff')

    # Convert the 'Experiment_Label' column to a factor, ordered by 'Percentage_Diff'
    plot_df['Experiment_Label'] = pd.Categorical(plot_df['Experiment_Label'], categories=plot_df['Experiment_Label'], ordered=True)

    # Define custom colors for groups
    custom_colors = {
        "Surgical": "#6B95DB",
        "FFP2": "#c9c9ff"
    }

    # Create the bar plot with grouped colors
    plot = (
        ggplot(plot_df, aes(x='Experiment_Label', y='Percentage_Diff', fill='Group')) +
        geom_bar(stat='identity', position='dodge') +
        geom_text(
            aes(label=plot_df['Percentage_Diff'].round(2)),
            va='center',
            color='black',
            size=10,
            nudge_y=1
        ) +
        theme_bw() +
        labs(
            x="Experiment",
            y="Percentage Difference (%)",
            fill="Mask Type"
        ) +
        theme(legend_position="bottom", legend_title=element_text(weight="bold", size=16), legend_text=element_text(size=14), legend_key_size=20, axis_text=element_text(size=12), axis_title=element_text(size=14, weight="bold")) +
        scale_fill_manual(values=custom_colors) # Add custom color scale
    )

    # Save the plot
    output_file = f"{output_file_prefix}_day_{day_x}_{baseline_exp}_vs_others.png"
    plot.save(filename="plots/" + output_file, width=10, height=6, dpi=300)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-experiment_dirs',
        type=str,
        nargs='+',
        help='List of directories for different experiments'
    )
    parser.add_argument(
        '-baseline_experiment',
        type=str,
        help='Directory of the baseline experiment'
    )
    parser.add_argument(
        '-day_x',
        type=int,
        help='The specific day to compare (e.g., day 50)'
    )
    parser.add_argument(
        '-experiment_labels',
        type=str,
        nargs='+',
        help='Custom labels for experiments (in the same order as experiment_dirs)'
    )
    args = parser.parse_args()

    os.system("mkdir -p plots")
    
    # Convert experiment_labels to a dictionary
    experiment_labels = None
    if args.experiment_labels:
        experiment_labels = dict(zip(args.experiment_dirs, args.experiment_labels))
    
    # Generate the percentage difference plot
    plot_percentage_difference_at_day_x(args.experiment_dirs, args.baseline_experiment, args.day_x, experiment_labels)


if __name__ == "__main__":
    main()
