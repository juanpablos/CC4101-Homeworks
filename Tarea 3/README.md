
# Set up
1. Virtual Env is recommended. A local virtual env in attached.
2. Run: source Env/LengT3/Scripts/activate  


# Run program
1. To run the program you must install the libraries in requirements.txt, or activate the virtual env as described above.
2. Run the program using: python Main.py "your expression"


# Tests
1. To run the tests you must have the python unittest library and the libraries in requirements.txt, or activate the virtual env as described above.
2. Run the tests using: python -m unittest test/test_parse.py test/test_aux.py test/test_interp.py test/test_run.py
