from subprocess import Popen, PIPE

from typing import List

class RProcess:
    """Provide a simple way to interact with an R process.
    
    Methods:
        execute(r_code)
        get_value(var_name, type="float")
    
    Example:
        >>> r_process = RProcess()
        >>> r_process.execute("x <- c(1, 2, 3)")
        >>> x = r_process.get_value("x")
        >>> print(x)
        [1, 2, 3]
    """
    # Special string to find the final line of the displayed output
    SPECIAL_MARKER_STRING: str = '"=== SPECIAL MARKER STRING ==="\n'

    def __init__(self, r_exec: str = "R") -> None:
        # Launch R with --vanilla to prevent influence from user's R configuration
        self.r_process: Popen = Popen([r_exec, "--vanilla"], 
            stdin=PIPE, stdout=PIPE, stderr=PIPE,
            # To interact with R using text instead of bytecode (>= Python 3.7)
            text=True)

        if self.r_process.stdin is None: raise AttributeError("stdin is None")
        if self.r_process.stdout is None: raise AttributeError("stdout is None")

        self.r_process.stdin.write(f"{RProcess.SPECIAL_MARKER_STRING}")
        self.r_process.stdin.flush()
        
        # Pass through all the lines displayed when starting R
        while True:
            line: str = self.r_process.stdout.readline()
            if line == f"[1] {RProcess.SPECIAL_MARKER_STRING}":
                break

    def execute(self, r_code: str | List[str]) -> None:
        """Execute R code without return value in the R process.
        
        This function is designed for executing R code without return value, 
        such as assignment statements.
        
        Args:
            r_code (str | List[str]): R code (single or multiple lines)
        """
        if self.r_process.stdin is None: raise AttributeError("stdin is None")
        if self.r_process.stdout is None: raise AttributeError("stdout is None")
        if self.r_process.stderr is None: raise AttributeError("stderr is None")
        
        if isinstance(r_code, str):
            self.r_process.stdin.write(f"{r_code}\n")
        else:
            for a_line_of_r_code in r_code:
                self.r_process.stdin.write(f"{a_line_of_r_code}\n")
        
        self.r_process.stdin.write(f"{RProcess.SPECIAL_MARKER_STRING}")
        self.r_process.stdin.flush()

        # Pass through all lines displayed as input and output
        while True:
            line: str = self.r_process.stdout.readline()
            if not line:
                error_message: str = self.r_process.stderr.readline()
                raise ValueError(error_message)
            if line == f"[1] {RProcess.SPECIAL_MARKER_STRING}":
                break

    def get_value(self, var_name: str, type: str = "float") -> list[float] | list[str]:
        """Get values of a simple vector variable assigned in the R process.
        
        This function is designed to retrieve values of a simple vector.
        Here, 'simple' refers to an atomic vector that is not a list and
        has no names and attributes. 
        
        Args:
            var_name (str): Variable name assigned in the R process.
            type (str): Type of the retuen value. It can be 'float' (default) or 'str'.
              
        Returns:
            list: Values of a simple vector variable.
        """
        if self.r_process.stdin is None: raise AttributeError("stdin is None")
        if self.r_process.stdout is None: raise AttributeError("stdout is None")
        
        self.r_process.stdin.write(f"{var_name}\n")
        self.r_process.stdin.write(f"{RProcess.SPECIAL_MARKER_STRING}")
        self.r_process.stdin.flush()
        
        # Ignore the first output line because it only displays the input var_name
        self.r_process.stdout.readline()

        result: List = []
        while True:
            # Assume only simple vectors, such as [1] 1 2 3 or [1] "a" "b" "c"
            line: str = self.r_process.stdout.readline()
            if line == f"> {RProcess.SPECIAL_MARKER_STRING}": 
                self.r_process.stdout.readline()
                break
            # Remove the first element as it represents index (e.g., [1])
            values: List[str] = line.split()[1:]
            result += values
        
        # Remove the extra double quotation marks surrounding strings
        result = [value.strip('"') for value in result]
        
        if type == "float":
            result = [float(value) for value in result]
        elif type != "str":
            raise ValueError(f"Type '{type}' must be either str or float")
          
        return result

    @staticmethod
    def to_R_notation(py_obj: str | List) -> str:
        """Convert a Python object to R's string notation.

        Example:
            >>> x = 'foobar'
            >>> y = RProcess.to_R_notation(x)
            >>> print(y)
            '"foobar"'
            >>> x = [1, 2, 3]
            >>> y = RProcess.to_R_notation(x)
            >>> print(y)
            'c(1, 2, 3)'

        Args:
            py_obj (str | List): A Python string or list. Numpy arrays are not supported.
      
        Returns:
            str: Character notation of R.
        """
        
        if isinstance(py_obj, str):
            return '"' + py_obj + '"'
        elif isinstance(py_obj, list):
            return "c(" + str(py_obj).strip("[]") + ")"
        else:
            raise TypeError(f"{type(py_obj)} is not supported.")

    def __del__(self) -> None:
        if self.r_process.stdin is None: raise AttributeError("stdin is None")
        self.r_process.stdin.close()
        self.r_process.terminate()
        self.r_process.wait()
