import pickle
import numpy as np
import pandas as pd
import pingouin as pg
import matplotlib.pyplot as plt
from scipy import stats
from matplotlib.lines import Line2D  # Import Line2D

if __name__ == "__main__":

    for exp_condition in ["without_ic", "with_ic"]:
        print("############################################")
        print("SHOWING RESULTS FOR", exp_condition)
        print("############################################")
        with open(f'res_dict_rw_{exp_condition}.pkl', 'rb') as f:
            res_dict = pickle.load(f)

        with open(f'res_dict_data_{exp_condition}.pkl', 'rb') as f:
            res_dict_data = pickle.load(f)

        with open(f'res_dict_bayes_{exp_condition}.pkl', 'rb') as f:
            res_dict_bayesian_model = pickle.load(f)

        # Create a figure with 2 rows and 2 columns (4 subplots)
        fig, axs = plt.subplots(2, 2, figsize=(10, 7), dpi=300)

        # Set up axes for 'slopes' and 'intercepts'
        ax_dict = {
            'slopes': [axs[0, 0], axs[1, 0]],
            'intercepts': [axs[0, 1], axs[1, 1]]
        }

        # Initialize position indices for plotting
        positions_dict = {'slopes': 0, 'intercepts': 0}

        # Define colors for conditions
        colors = ['purple', 'khaki', 'pink', 'blue']

        # Initialize legend elements list
        legend_elements = []

        # Loop over selectors ('slopes' and 'intercepts')
        for selector in ['slopes', 'intercepts']:
            ax1, ax2 = ax_dict[selector]
            i = positions_dict[selector]  # Position index for plotting

            for condition, color in zip(res_dict.keys(), colors):

                # Extract data for Block Half 1 and Block Half 2
                data_list_1 = [
                    res_dict[condition][1][selector],
                    res_dict_data[condition][1][selector],
                    res_dict_bayesian_model[condition][1][selector]
                ]
                data_list_2 = [
                    res_dict[condition][2][selector],
                    res_dict_data[condition][2][selector],
                    res_dict_bayesian_model[condition][2][selector]
                ]

                # Convert lists to NumPy arrays
                data_list_1 = list(map(lambda x: np.array(x), data_list_1))
                data_list_2 = list(map(lambda x: np.array(x), data_list_2))

                # Plot boxplots for Block Half 1
                bp1 = ax1.boxplot(
                    data_list_1,
                    positions=[i, i+1, i+2],
                    widths=0.6,
                    patch_artist=True,
                    showfliers=False
                )

                # Plot boxplots for Block Half 2
                bp2 = ax2.boxplot(
                    data_list_2,
                    positions=[i, i+1, i+2],
                    widths=0.6,
                    patch_artist=True,
                    showfliers=False
                )

                # Color the boxplots
                for bp in [bp1, bp2]:
                    for element in ['boxes', 'whiskers', 'caps', 'medians']:
                        plt.setp(bp[element], color=color)
                    for patch in bp['boxes']:
                        patch.set_facecolor(color)
                        patch.set_alpha(0.5)

                # Create DataFrame for statistical tests
                df_1 = pd.DataFrame({
                    'value': np.concatenate(data_list_1),
                    'label': [0]*len(data_list_1[0]) + [1]*len(data_list_1[1]) + [2]*len(data_list_1[2])
                })
                df_2 = pd.DataFrame({
                    'value': np.concatenate(data_list_2),
                    'label': [0]*len(data_list_2[0]) + [1]*len(data_list_2[1]) + [2]*len(data_list_2[2])
                })

                # Perform ANOVA for Block Half 1 and Block Half 2
                res_1 = pg.anova(data=df_1, dv='value', between='label', detailed=True)
                res_2 = pg.anova(data=df_2, dv='value', between='label', detailed=True)

                print(f"Results for {condition} - {selector}")
                print("ANOVA res_1")
                print(res_1)
                print("ANOVA res_2")
                print(res_2)

                # Perform pairwise t-tests with Bonferroni correction
                pairwise_results_1 = pg.pairwise_ttests(
                    dv='value', between='label', padjust='bonferroni', data=df_1
                )
                pairwise_results_2 = pg.pairwise_ttests(
                    dv='value', between='label', padjust='bonferroni', data=df_2
                )

                print(f"Pairwise t-tests for {condition} - {selector}")
                print("pairwise_results_1")
                print(pairwise_results_1)
                print("pairwise_results_2")
                print(pairwise_results_2)

                # Collect legend elements only once
                if selector == 'slopes':
                    legend_elements.append(Line2D([0], [0], color=color, lw=4, label=condition))

                # Function to plot significance stars above significant models
                def plot_significance_stars(ax, res_anova, pairwise_results, positions):
                    if res_anova['p-unc'][0] < 0.05:
                        # Check pairwise comparisons between data (label 1) and models (labels 0 and 2)
                        significant_pairs = pairwise_results[
                            (((pairwise_results['A'] == 1) & (pairwise_results['B'].isin([0, 2]))) |
                             ((pairwise_results['B'] == 1) & (pairwise_results['A'].isin([0, 2])))) &
                            (pairwise_results['p-unc'] < 0.05)
                        ]
                        if not significant_pairs.empty:
                            for idx, row in significant_pairs.iterrows():
                                # Get the model label that is significantly different from data
                                model_label = row['B'] if row['A'] == 1 else row['A']
                                # Map the label to position
                                position = positions[int(model_label)]
                                # Plot significance star above the model
                                ax.text(position, ax.get_ylim()[1]*0.9, '*', ha='center', size=15)
                        else:
                            print(f"No significant pairwise differences between data and models for {condition} - {selector}")
                    else:
                        print(f"No significant ANOVA result for {condition} - {selector}")

                # Positions of the boxplots
                positions = [i, i+1, i+2]  # [RW, Data, Bayes]

                # Plot significance stars for Block Half 1
                plot_significance_stars(ax1, res_1, pairwise_results_1, positions)

                # Plot significance stars for Block Half 2
                plot_significance_stars(ax2, res_2, pairwise_results_2, positions)

                # Update position index
                i += 4  # Increase by 4 to separate groups visually

            # Set titles and labels for the subplots
            ax1.set_title(f'{selector.capitalize()} Block Half 1')
            ax2.set_title(f'{selector.capitalize()} Block Half 2')

            # Set x-ticks and labels for the subplots
            xticks = []
            xticklabels = []
            num_conditions = len(res_dict.keys())
            for idx in range(num_conditions):
                pos = idx * 4 + 1  # Center position for each group
                xticks.extend([pos - 1, pos, pos + 1])
                xticklabels.extend(['RW', 'Data', 'Bayes'])
            ax1.set_xticks(xticks)
            ax1.set_xticklabels(xticklabels, fontsize=7)
            ax2.set_xticks(xticks)
            ax2.set_xticklabels(xticklabels, fontsize=7)

            # Set Y-axis limits for consistency
            for ax in [ax1, ax2]:
                ax.set_ylim(-3.5, 3.5)

            # Update position index in the dictionary
            positions_dict[selector] = i  # Update for the next selector

        # Adjust layout and add legend
        fig.legend(handles=legend_elements, loc='lower center', ncol=4, bbox_to_anchor=(0.5, 0.02))

        plt.tight_layout(rect=[0, 0.05, 1, 1])  # Adjust layout to make room for the legend
        plt.savefig(f'plots/combined_{exp_condition}.png')
        plt.close()