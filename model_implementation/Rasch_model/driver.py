import subprocess

if __name__ == "__main__":
    subprocess.call(
        ["R --vanilla --args model_implementation/Rasch_model/Rasch.R"], shell=True)
