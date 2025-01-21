import os
import pandas as pd
from plotnine import ggplot, aes, geom_line, labs, scale_color_manual, theme_bw, facet_wrap, theme, element_text

def plot_seir_states_with_average(base_dirs, scenario_names, custom_order, filename='evolution.csv', output_dir='plots', output_file='seir_states_together.png'):
    # Define colors for scenarios
    scenario_colors = [
        '#559e83',  # No Countermeasures (green)
        '#6B95DB',  # Surgical 20% (blue)
        '#ff8b94',  # Surgical 40% (red)
        '#f4a261',  # FFP2 20% (orange)
        '#e76f51',  # FFP2 40% (coral)
        '#494949',  # Surgical 80% (black/gray)
        '#b56576'   # FFP2 80% (pink)
    ]
    
    # Map each base_dir (scenario) to a color
    color_map = {}
    aggregated_data = pd.DataFrame()

    for idx, (base_dir, scenario_name) in enumerate(zip(base_dirs, scenario_names)):
        all_seed_data = pd.DataFrame()

        # Collect data from all subdirectories containing evolution.csv
        for sub_dir in os.listdir(base_dir):
            full_path = os.path.join(base_dir, sub_dir, filename)
            if os.path.isfile(full_path):  # Check if the file exists
                seed_data = pd.read_csv(full_path)
                all_seed_data = pd.concat([all_seed_data, seed_data], ignore_index=True)

        # Compute the average across all seeds
        if not all_seed_data.empty:
            numeric_columns = ['Susceptible', 'Exposed', 'Infected', 'Recovered']
            average_data = all_seed_data.groupby('Day')[numeric_columns].mean().reset_index()
            average_data['Scenario'] = scenario_name
            aggregated_data = pd.concat([aggregated_data, average_data], ignore_index=True)

            # Assign a color to the scenario
            if scenario_name not in color_map:
                color_map[scenario_name] = scenario_colors[idx % len(scenario_colors)]

    # Ensure the output directory exists
    os.makedirs(output_dir, exist_ok=True)

    # Reorder the 'Scenario' column according to the custom order
    aggregated_data['Scenario'] = pd.Categorical(aggregated_data['Scenario'], categories=custom_order, ordered=True)

    # Reshape the data for facet_wrap
    melted_data = aggregated_data.melt(id_vars=['Day', 'Scenario'], value_vars=['Susceptible', 'Exposed', 'Infected', 'Recovered'],
                                       var_name='State', value_name='Population')

    # Define custom order for facets (S, E, I, R)
    state_order = ['Susceptible', 'Exposed', 'Infected', 'Recovered']
    melted_data['State'] = pd.Categorical(melted_data['State'], categories=state_order, ordered=True)

    # Create the plot with facet_wrap
    plot = (ggplot(melted_data, aes(x='Day', y='Population', color='Scenario')) +
            geom_line(size=1, alpha=0.8) +
            scale_color_manual(values=color_map) +
            facet_wrap('~State', ncol=2, scales='free_y') +  # Facet by state (S, E, I, R)
            theme_bw() +
            labs(x='Day', y='Population', color='Scenario') +
            theme(legend_position="bottom", legend_title=element_text(weight="bold", size=16), legend_text=element_text(size=14), legend_key_size=20, axis_text=element_text(size=12), axis_title=element_text(size=14, weight="bold")))

    # Save the plot
    output_path = os.path.join(output_dir, output_file)
    plot.save(output_path, width=16, height=10, dpi=300)
    print(f"Faceted plot saved at {output_path}")

if __name__ == '__main__':
    scenarios = [
        "../results/AlarmNoCountermeasures",
        "../results/AlarmSurgical20FromDay4",
        "../results/AlarmSurgical40FromDay4",
        "../results/AlarmFFP220FromDay4",
        "../results/AlarmFFP240FromDay4",
        "../results/AlarmSurgical80FromDay4",
        "../results/AlarmFFP280FromDay4",
    ]
    
    scenario_names = [
        "No Countermeasures",
        "Surgical 20%",
        "Surgical 40%",
        "FFP2 20%",
        "FFP2 40%",
        "Surgical 80%",
        "FFP2 80%",
    ]
    
    custom_order = [
        "No Countermeasures",
        "Surgical 20%",
        "Surgical 40%",
        "Surgical 80%",
        "FFP2 20%",
        "FFP2 40%",
        "FFP2 80%",
    ]
    
    plot_seir_states_with_average(base_dirs=scenarios, scenario_names=scenario_names, custom_order=custom_order, output_file="seir_states_together.png")