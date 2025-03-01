from online_shopping_cart.user.user_authentication import UserAuthenticator
from online_shopping_cart.user.user_interface import UserInterface
from online_shopping_cart.user.user_data import UserDataManager

########################
# USER LOGIN FUNCTIONS #
########################

def is_quit(input_argument: str) -> bool:
    if isinstance(input_argument, str):
        return input_argument.lower() == 'q'
    else:
        return False

def is_yes(input_argument: str) -> bool:
    if isinstance(input_argument, str):
        return input_argument.lower() == 'y' or input_argument.lower() == 'yes'
    else:
        return False

def is_password_valid(input_argument: str) -> bool:
    if not isinstance(input_argument, str):
        return False
    if len(input_argument) < 8:
        return False
    if not any(char.isupper() for char in input_argument):
        return False
    if not any(not char.isalnum() for char in input_argument):  # isalnum() 检查字符是否是字母或数字
        return False
    return True

def login() -> dict[str, str | float] | None:
    username: str = UserInterface.get_user_input(prompt="Enter your username (or 'q' to quit): ")
    if is_quit(input_argument=username):
        exit(0)  # The user has quit

    password: str = UserInterface.get_user_input(prompt="Enter your password (or 'q' to quit): ")
    if is_quit(input_argument=password):
        exit(0)   # The user has quit

    if not isinstance(username, str) or not isinstance(password, str):
        print("invalid date type, str is expected.")
        return None
    username = username.replace(" ", "")
    is_authentic_user: dict[str, str | float] = UserAuthenticator().login(
        username=username,
        password=password,
        data=UserDataManager.load_users()
    )
    if is_authentic_user is not None:
        return is_authentic_user

    # TODO: Task 1: prompt user to register when not found
    users = UserDataManager.load_users()
    user_exist = False
    for user in users:
        if user["username"] == username:
            user_exist = True
            break
    if username=="":
        print("Username is empty, please try again.")
        return None
    if not user_exist:
        print("Username: " + username + " does not exist")
        answer: str = UserInterface.get_user_input(prompt="Do you want to create a new user? (y/n): ")
        if is_yes(answer):
            while True:
                password: str = UserInterface.get_user_input(prompt="Please enter your password: ")
                if is_password_valid(input_argument=password):
                    new_user = {"password": password, "username": username, "wallet": 0}
                    users.append(new_user)
                    UserDataManager.save_users(users)
                    print("New user created!")
                    break
                else:
                    print("Invalid password! Please try again. At least 8 characters long, and at least one capital letter and one special symbol.")
                    continue

    return None
