import pytest
from unittest.mock import patch
from online_shopping_cart.user.user_login import login
from online_shopping_cart.user.user_data import UserDataManager
from online_shopping_cart.user.user_interface import UserInterface


@pytest.fixture
def mock_users():
    return [
        {'username': 'existing_user', 'password': 'ValidPass123!', 'wallet': 100.0}
    ]


@pytest.fixture
def mock_load_users(mock_users):
    with patch.object(UserDataManager, 'load_users', return_value=mock_users):
        yield


@pytest.fixture
def mock_save_users():
    with patch.object(UserDataManager, 'save_users') as mock_save:
        yield mock_save


@pytest.mark.parametrize("username_input, password_input", [
    (12345, 'ValidPass123!'),  # Username is int
    ('existing_user', 12345.678),  # Password is float
    (['user', 'name'], 'ValidPass123!'),  # Username is list
    ('existing_user', ['pass', 'word']),  # Password is list
])
# Invalid Input Type
def test_login_invalid_input_types(username_input, password_input, mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[username_input, password_input]):
        user = login()
        assert user is None  # Expecting the function to handle invalid types gracefully


# Valid Input Type
# Bug fixed list:
#   1. can register account with empty username
#   2. username staring or ending with whitespaces will be regarded as new username

# case 1: valid login
def test_login_valid_credentials(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['existing_user', 'ValidPass123!']):
        user = login()
        assert user is not None
        assert user['username'] == 'existing_user'


# case 2: wrong password
def test_login_incorrect_password(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['existing_user', 'WrongPassword']):
        user = login()
        assert user is None


# case 3: non-existent username & register a new one
def test_login_register_new_user_valid_password(mock_load_users, mock_save_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[
        'new_user',  # Username input
        'ValidPass123!',  # password input
        'y',  # User chooses to register
        'ValidPass123!'  # Valid password
    ]):
        user = login()
        assert user is None  # After registration, login() returns None
        # Verify that save_users was called
        mock_save_users.assert_called_once()


# case 4: non-existent username & don't register
def test_login_user_declines_registration(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[
        'unknown_user',  # Username input
        'ValidPass123!',  # password input
        'n'  # User declines to register
    ]):
        user = login()
        assert user is None


# case 5: user quit when entering username
def test_login_user_quits_at_username():
    with patch.object(UserInterface, 'get_user_input', side_effect=['q']), \
            pytest.raises(SystemExit):
        login()


# case 6: user quit when entering password
def test_login_user_quits_at_password(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['existing_user', 'q']), \
            pytest.raises(SystemExit):
        login()


# case 7: register with invalid password, and with valid password at the second time
def test_login_register_new_user_invalid_password(mock_load_users, mock_save_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[
        'new_user',  # Username input
        'ValidPass123!',
        'y',  # User chooses to register
        'invalid',  # Password that fails validation
        'ValidPass123!'  # Valid password
    ]):
        user = login()
        assert user is None
        mock_save_users.assert_called_once()


# case 8: empty username and password
def test_login_empty_username_and_password(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['', '']):
        user = login()
        assert user is None


# case 9: empty username
def test_login_empty_username(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['', 'ValidPass123!']):
        user = login()
        assert user is None


# case 10: empty password
def test_login_empty_password(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=['existing_user', '']):
        user = login()
        assert user is None


# case 11: username and password with leading/trailing whitespaces
def test_login_username_password_with_whitespaces(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[' existing_user ', ' ValidPass123! ']):
        user = login()
        assert user is None  # username don't contain whitespace, but password can


# case 12: username with leading/trailing whitespaces
def test_login_username_with_whitespaces(mock_load_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[' existing_user ', 'ValidPass123!']):
        user = login()
        assert user is not None  # username don't contain whitespace


# case 13: user inputs 'yes' in different cases for registration
@pytest.mark.parametrize("yes_input", ['Y', 'y', 'Yes', 'YES', 'yeS'])
def test_login_user_registers_with_various_yes_inputs(yes_input, mock_load_users, mock_save_users):
    with patch.object(UserInterface, 'get_user_input', side_effect=[
        'new_user',  # username input
        'ValidPass123!',  # password input
        yes_input,  # user inputs 'yes' in various cases
        'ValidPass123!'  # valid password
    ]):
        user = login()
        assert user is None
        mock_save_users.assert_called_once()
