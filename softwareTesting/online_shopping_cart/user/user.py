################
# USER CLASSES #
################


class User:
    """
    User class to represent user information
    """

    def __init__(self, name, wallet) -> None:
        if not isinstance(name, str) or not name:
            raise TypeError("User name must be a non-empty string")
        if not isinstance(wallet, (int, float)) or wallet is None:
            raise TypeError("User wallet must be a number")
        self.name: str = name
        self.wallet: float = wallet
