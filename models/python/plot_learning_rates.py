import pickle
import matplotlib.pyplot as plt
import os
import numpy as np

# Function to read a pickle file and return its contents
def read_pickle(file_path):
    with open(file_path, 'rb') as file:
        data = pickle.load(file)
    return data

# Directory containing the pickle files
directory = 'learning_rates'  # Change this to the path where your pickle files are stored

# Initialize containers for grouping the data
with_data = {'a': [], 'b': [], 'c': []}  # with feedback
without_data = {'a': [], 'b': [], 'c': []}  # without feedback

# Loop through each file in the directory
for filename in os.listdir(directory):
    if filename.endswith('.pkl') or filename.endswith('.pickle'):
        file_path = os.path.join(directory, filename)
        
        # Read the data from the pickle file
        data = read_pickle(file_path)
        
        # Check if the data is a dictionary
        if isinstance(data, dict):
            for key, value in data.items():
                if isinstance(value, list):
                    # Group the data based on file name pattern
                    if 'Without' in filename:
                        if '_a_' in filename:
                            without_data['a'].extend(value)
                        elif '_b_' in filename:
                            without_data['b'].extend(value)
                        elif '_c_' in filename:
                            without_data['c'].extend(value)
                    else:
                        if '_a_' in filename:
                            with_data['a'].extend(value)
                        elif '_b_' in filename:
                            with_data['b'].extend(value)
                        elif '_c_' in filename:
                            with_data['c'].extend(value)
                else:
                    print(f"The value for key '{key}' in {filename} is not a list. Skipping.")
        else:
            print(f"The data in {filename} is not a dictionary. Skipping.")

# Colors for each learning rate category
colors = {'a': 'blue', 'b': 'green', 'c': 'red'}
key_to_label = {'a': r'$\lambda_0$', 'b': r'$\lambda_1$', 'c': r'$\lambda_2$'}

# Plot histograms
fig, axs = plt.subplots(1, 2, figsize=(14, 6), sharey=True)

# Plot for "With" data
for key in with_data:
    axs[0].hist(np.log10(with_data[key]), bins=30, alpha=0.7, edgecolor='black', 
                color=colors[key], label=f'{key_to_label[key]}')
axs[0].set_title('Full Feedback')
axs[0].set_xlabel(r'$log_{10}$ Learning rate')
axs[0].set_ylabel('Frequency')
axs[0].legend()
axs[0].grid(True)

# Plot for "Without" data
for key in without_data:
    axs[1].hist(np.log10(without_data[key]), bins=30, alpha=0.7, edgecolor='black', 
                color=colors[key], label=f'{key_to_label[key]}')
axs[1].set_title('Noisy Condition')
axs[1].set_xlabel(r'$log_{10}$ Learning rate')
axs[1].legend()
axs[1].grid(True)

# Print mean and stds for each learning rate
print("With feedback:")
for key in with_data:
    print(f"Learning rate {key}: Mean: {np.mean(with_data[key]):.4f}, Std: {np.std(with_data[key], ddof=1):.4f}")
    
print("\nWithout feedback:")
for key in without_data:
    print(f"Learning rate {key}: Mean: {np.mean(without_data[key]):.4f}, Std: {np.std(without_data[key], ddof=1):.4f}")

plt.tight_layout()
plt.savefig('learning_rates_histogram.png')