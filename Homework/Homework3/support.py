




#########################################################################################################################
import matplotlib.pyplot as plt

def plot_moving_stats(df, cols, window_size=12, line_color="#A22538"):
    """
    Plots time series data with moving average, moving standard deviation,
    and a horizontal line for the overall mean.

    Parameters:
    - df: pandas DataFrame with a time-based index.
    - cols: list of column names to plot.
    - window_size: rolling window size for moving average and std (default: 12).
    - line_color: color for the original time series line (default: red).
    """
    n_vars = len(cols)
    n_cols = 2  # Number of plots per row
    n_rows = (n_vars + 1) // n_cols  # Compute number of rows needed

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(10, 2.8 * n_rows), sharex=True)
    axes = axes.flatten()

    for i, column in enumerate(cols):
        ax = axes[i]
        series = df[column]

        # Compute moving average and std
        moving_mean = series.rolling(window=window_size).mean()
        moving_std = series.rolling(window=window_size).std()
        
        # Overall mean
        mean_value = series.mean()

        # Plot
        ax.plot(df.index, series, color=line_color, linewidth=1.2, label=f"{column}")
        ax.axhline(mean_value, color='black', linestyle='--', linewidth=1, label='Overall Mean')
        ax.plot(df.index, moving_mean, color='black', linestyle='-', linewidth=1.5, label="Moving Average")
        ax.fill_between(df.index,
                        moving_mean - moving_std,
                        moving_mean + moving_std,
                        color='grey', alpha=0.3, label='±1 Moving Std Dev')

        ax.set_title(f"{column}", fontsize=10, fontweight='bold')
        ax.tick_params(axis='both', labelsize=8)
        ax.legend(fontsize=6, loc='best')
        ax.set_xlabel("")
        ax.set_ylabel("")

    # Remove unused subplots
    for j in range(i + 1, len(axes)):
        fig.delaxes(axes[j])

    plt.tight_layout()
    plt.show()
 #########################################################################################################################

from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import matplotlib.pyplot as plt

def plot_var_diagnostics(model, variables=None, lags=15, color="#d62728", figsize=(8, 3)):
    """
    Plot residuals, ACF, and PACF for each endogenous variable in a VAR model.

    Parameters:
    - model: fitted VARResults object from statsmodels
    - variables: list of variable names (default: all model variables)
    - lags: number of lags for ACF/PACF plots
    - color: color used in the plots (default: red)
    - figsize: tuple indicating figure size for residual plots
    """
    if variables is None:
        variables = model.resid.columns.tolist()

    for var in variables:
        if var not in model.resid.columns:
            print(f"Warning: Variable '{var}' not found in model residuals. Skipping.")
            continue

        residuals = model.resid[var].dropna()

        # Residual plot
        plt.figure(figsize=figsize)
        plt.plot(residuals, color=color)
        plt.title(f"Residuals - {var}", fontsize=12, fontweight='bold')
        plt.xlabel("Time")
        plt.ylabel("Residuals")
        plt.grid(True, linestyle='--', alpha=0.5)
        plt.tight_layout()
        plt.show()

        # ACF and PACF plots
        fig, axs = plt.subplots(2, 1, figsize=(8, 5))
        plot_acf(residuals, lags=lags, ax=axs[0], color=color)
        axs[0].set_title(f"ACF - {var}", fontsize=11)
        plot_pacf(residuals, lags=lags, ax=axs[1], color=color)
        axs[1].set_title(f"PACF - {var}", fontsize=11)
        plt.tight_layout()
        plt.show()
#########################################################################################################################
