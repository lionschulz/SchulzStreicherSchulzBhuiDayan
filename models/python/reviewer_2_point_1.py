import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from IPython import embed
from tqdm import tqdm
from pprint import pprint

if __name__ == "__main__":

    glob_df = pd.read_csv('data/df_main_task.csv')
    glob_df = glob_df[glob_df['block_stage'] != 'station_only']
    """ print(glob_df.keys()) Index(['completion_code', 'condition', 'news_station_first',
       'with_independent_council', 'rt', 'experiment_stage', 'response',
       'accuracy', 'plugin_type', 'num_blue_indep', 'num_blue_news',
       'source_type', 'block_stage', 'block_number', 'expert_look_indicator',
       'correct_state', 'trial_number_within_block', 'trial_number',
       'slider_start', 'quadratic_score', 'total_quadratic_score', 'date',
       'pilot_type', 'response_absolute', 'block_half', 'block_quarter'],
      dtype='object') """
    glob_df.groupby(by="condition")['response_absolute'].hist(alpha=0.5)

    """Permutation Test"""
    # Function to perform the permutation test
    def permutation_test(group1, group2, n_permutations=10000, subsample=None):
        if subsample is not None:
            group1 = np.random.choice(group1, subsample)
            group2 = np.random.choice(group2, subsample)
        observed_diff = np.mean(group1) - np.mean(group2)
        combined_data = np.concatenate([group1, group2])
        pooled_std = np.sqrt( ((len(group1 - 1) * np.std(group1, ddof=1)**2 \
                                + (len(group2) - 1) * np.std(group2, ddof=1)**2)) \
                                    / (len(group1) + len(group2) - 2) )
        def permute_and_get_diff(combined, n1):
            np.random.shuffle(combined)
            new_group1 = combined[:n1]
            new_group2 = combined[n1:]
            return np.mean(new_group1) - np.mean(new_group2)

        perm_diffs = np.array([permute_and_get_diff(combined_data, len(group1)) 
                            for _ in range(n_permutations)])
        

        p_value = np.mean(np.abs(perm_diffs) >= np.abs(observed_diff))
        cohens_d = observed_diff / pooled_std
        return observed_diff, p_value, cohens_d

    # Perform pairwise comparisons
    results = {}
    for i, group in glob_df.groupby('condition')['response_absolute']:
        results[i] = {}
        for j, group2 in glob_df.groupby('condition')['response_absolute']:
            if i != j:
                observed_diff, p_value, cohens_d = permutation_test(group, group2)
                results[i][j] = (observed_diff, p_value, cohens_d)
    
    print(results)
    p_values = np.zeros(( np.unique(glob_df['source_type']).shape[0],  np.unique(glob_df['source_type']).shape[0]))
    cohens_ds = np.zeros(( np.unique(glob_df['source_type']).shape[0],  np.unique(glob_df['source_type']).shape[0]))
    comparison = []

    for i, (group_name, group_df) in tqdm(enumerate(glob_df.groupby(by='source_type')['response_absolute'])):
        for j, (group2_name, group2_df) in enumerate(glob_df.groupby(by='source_type')['response_absolute']):
            comparison.append((group_name, group2_name))
            if i != j:
                observed_diff, p_value, cohens_d = permutation_test(group_df, group2_df)
                p_values[i][j] = p_value
                cohens_ds[i][j] = cohens_d
    
    """Check for interactions"""

    interaction_results = {}
    interaction_cohens = {}
    interaction_comparison = {}
    for condition in glob_df['condition'].unique():
        interaction_results[condition] = np.zeros((glob_df['source_type'].unique().shape[0], glob_df['source_type'].unique().shape[0]))
        interaction_cohens[condition] = np.zeros((glob_df['source_type'].unique().shape[0], glob_df['source_type'].unique().shape[0]))
        condition_df = glob_df[glob_df['condition'] == condition]
        interaction_comparison[condition] = []
        for i, (group_name, group_df) in enumerate(condition_df.groupby(by='source_type')['response_absolute']):
            for j, (group2_name, group2_df) in enumerate(condition_df.groupby(by='source_type')['response_absolute']):
                if i != j:
                    observed_diff, p_value, cohens_d = permutation_test(group_df, group2_df)
                    interaction_results[condition][i][j] = p_value
                    interaction_cohens[condition][i][j] = cohens_d
                    interaction_comparison[condition].append((group_name, group2_name))

    print("Difference between conditions")
    print(f"sample size without noise: {len(glob_df[glob_df['condition'] == "Without"])}")
    print(f"sample size with noise: {len(glob_df[glob_df['condition'] == "With"])}")
    pprint(results)

    print("Difference between source types")
    print(f"sample size helpful: {len(glob_df[glob_df['source_type'] == "helpful"])}")
    print(f"sample size bluebias: {len(glob_df[glob_df['source_type'] == "bluebias"])}")
    print(f"sample size opposite: {len(glob_df[glob_df['source_type'] == "opposite"])}")
    print(f"sample size random: {len(glob_df[glob_df['source_type'] == "random"])}")

    print(f"p-values: ")
    pprint(p_values)
    print(f"cohens_d: ")
    pprint(cohens_ds)
    print(f"comparison: ")
    pprint(comparison)

    print("sample sizes full feedback")
    print(f"sample size helpful: {len(glob_df[(glob_df['source_type'] == "helpful") & (glob_df['condition'] == "Without")])}")
    print(f"sample size bluebias: {len(glob_df[(glob_df['source_type'] == "bluebias") & (glob_df['condition'] == "Without")])}")
    print(f"sample size opposite: {len(glob_df[(glob_df['source_type'] == "opposite") & (glob_df['condition'] == "Without")])}")
    print(f"sample size random: {len(glob_df[(glob_df['source_type'] == "random") & (glob_df['condition'] == "Without")])}")

    print("sample sizes no feedback")
    print(f"sample size helpful: {len(glob_df[(glob_df['source_type'] == "helpful") & (glob_df['condition'] == "With")])}")
    print(f"sample size bluebias: {len(glob_df[(glob_df['source_type'] == "bluebias") & (glob_df['condition'] == "With")])}")
    print(f"sample size opposite: {len(glob_df[(glob_df['source_type'] == "opposite") & (glob_df['condition'] == "With")])}")
    print(f"sample size random: {len(glob_df[(glob_df['source_type'] == "random") & (glob_df['condition'] == "With")])}")

    print("Interaction between conditions")
    pprint(interaction_results)
    print("Interaction between conditions cohens d")
    pprint(interaction_cohens)
    print("Interaction between conditions comparison")
    pprint(interaction_comparison)

    breakpoint()