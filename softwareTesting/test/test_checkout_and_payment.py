import pytest
from collections import deque
from online_shopping_cart.checkout.checkout_process import checkout_and_payment
from online_shopping_cart.user.user_data import UserDataManager
from online_shopping_cart.user.user_interface import UserInterface
from online_shopping_cart.checkout.shopping_cart import ShoppingCart
from online_shopping_cart.product.product_data import get_products


class ExitException(Exception):
    pass


def reset_globals():
    from online_shopping_cart.checkout import checkout_process
    checkout_process.global_products = get_products()
    checkout_process.global_cart = ShoppingCart()

# 自动执行的 fixture，在每个测试前重置全局状态
@pytest.fixture(autouse=True)
def reset_globals_fixture():
    reset_globals()

# helper function
def run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet):
    # 创建输入的队列
    inputs = deque(user_inputs)

    # 定义自定义异常
    class OutOfInputsError(Exception):
        pass

    # 添加调用计数器，防止无限循环
    max_calls = 100
    call_count = [0]

    def mock_get_user_input(*args, **kwargs):
        call_count[0] += 1
        if call_count[0] > max_calls:
            raise Exception("Too many calls to get_user_input")
        if inputs:
            return inputs.popleft()
        else:
            prompt = kwargs.get('prompt', '')
            raise OutOfInputsError(f"Ran out of user inputs when prompted with: {prompt}")

    monkeypatch.setattr(UserInterface, 'get_user_input', mock_get_user_input)

    # 模拟 UserDataManager.load_users 和 save_users
    users_data = [{'username': login_info.get('username', 'default_user'), 'password': 'pass',
                   'wallet': login_info.get('wallet', 0.0)}]

    def mock_load_users():
        return users_data.copy()

    def mock_save_users(users):
        users_data.clear()
        users_data.extend(users)

    monkeypatch.setattr(UserDataManager, 'load_users', mock_load_users)
    monkeypatch.setattr(UserDataManager, 'save_users', mock_save_users)

    # 模拟 exit 函数，抛出自定义异常以中断测试流程
    def mock_exit(code):
        raise ExitException("Exit called")

    monkeypatch.setattr('builtins.exit', mock_exit)

    # 运行函数并捕获异常
    try:
        checkout_and_payment(login_info)
    except OutOfInputsError as e:
        pytest.fail(str(e))
    except ExitException:
        pass  # 这是预期的，用于正常结束测试

    # 验证钱包余额
    assert users_data[0]['wallet'] == expected_wallet

######################
# 测试用例开始
######################

# 获取实际的产品列表，用于测试
products = get_products()

# 创建产品编号到价格的映射
product_prices = {str(index + 1): product.price for index, product in enumerate(products)}
product_names = {str(index + 1): product.name for index, product in enumerate(products)}

##############################
# 等价类 EC1: 无效的登录信息    Invalid  Login  Info
##############################

def test_invalid_login_missing_username(monkeypatch):
    """TC1: invalid login info (missing username)"""
    login_info = {'wallet': 100.0}
    monkeypatch.setattr(UserInterface, 'get_user_input', lambda *args, **kwargs: '')
    with pytest.raises(KeyError):
        checkout_and_payment(login_info)

def test_invalid_login_missing_wallet(monkeypatch):
    """TC2: invalid login info (missing wallet)"""
    login_info = {'username': 'user1'}
    monkeypatch.setattr(UserInterface, 'get_user_input', lambda *args, **kwargs: '')
    with pytest.raises(KeyError):
        checkout_and_payment(login_info)

def test_wallet_as_string(monkeypatch):
    """TC3: invalid input types (wallet as string)"""
    login_info = {'username': 'user1', 'wallet': '500.0'}
    monkeypatch.setattr(UserInterface, 'get_user_input', lambda *args, **kwargs: '')
    with pytest.raises(TypeError):
        checkout_and_payment(login_info)

def test_username_as_integer(monkeypatch):
    """TC4: invalid input types (username as integer)"""
    login_info = {'username': 12345, 'wallet': 500.0}
    monkeypatch.setattr(UserInterface, 'get_user_input', lambda *args, **kwargs: '')
    with pytest.raises(TypeError):
        checkout_and_payment(login_info)

def test_no_username_in_system(monkeypatch):
    """TC5: invalid login info (username not in system)"""
    login_info = {'username': 'unknown_user', 'wallet': 500.0}
    user_inputs = ['l', 'y']  # 退出并确认
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC2: 钱包金额     About wallet
##############################

def test_wallet_negative(monkeypatch):
    """TC6: wallet amount is negative"""
    login_info = {'username': 'user1', 'wallet': -100.0}
    user_inputs = ['l', 'y']  # 退出并确认
    expected_wallet = -100.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_wallet_zero_attempt_purchase(monkeypatch):
    """TC7 wallet amount is zero, attempt to purchase"""
    login_info = {'username': 'user1', 'wallet': 0.0}
    user_inputs = ['1', 'c', 'n', 'n', 'l', 'y']  # 尝试购买，查看购物车，结账，退出并确认
    expected_wallet = 0.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_wallet_zero_no_purchase(monkeypatch):
    """TC8: wallet amount is zero, no purchase"""
    login_info = {'username': 'user1', 'wallet': 0.0}
    user_inputs = ['l', 'y']  # 直接退出
    expected_wallet = 0.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_insufficient_funds(monkeypatch):
    """TC9: wallet amount is positive but insufficient for purchase"""
    login_info = {'username': 'user1', 'wallet': 5.0}
    user_inputs = ['6', 'c', 'n', 'n', 'l', 'y']
    expected_wallet = 5.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_exact_funds_purchase(monkeypatch):
    """TC10: wallet amount is exactly sufficient for purchase"""
    login_info = {'username': 'user1', 'wallet': 10.0}
    user_inputs = ['6', 'c', 'y', 'l', 'y']
    expected_wallet = 0.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_sufficient_funds_purchase(monkeypatch):
    """TC11: wallet amount is more than sufficient for purchase"""
    login_info = {'username': 'user1', 'wallet': 50.0}
    user_inputs = ['6', 'c', 'y', 'l', 'y']
    expected_wallet = 40.0  # 50 - 10
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC8: 有效的产品选择         plain Valid Input
##############################

def test_valid_product_purchase(monkeypatch):
    """TC12: valid product selection and successful checkout"""
    login_info = {'username': 'user1', 'wallet': 100.0}
    user_inputs = ['2', 'c', 'y', 'l', 'y']  # 购买产品2，结账，退出
    expected_wallet = 100.0 - product_prices['2']
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC4: 产品选择异常    Abnormal product selection
##############################

def test_invalid_product_selection(monkeypatch):
    """TC13: invalid product selection"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['999', 'l', 'y']
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_product_out_of_stock(monkeypatch):
    """TC14: product out of stock"""
    login_info = {'username': 'user1', 'wallet': 5000.0}
    # Deplete the stock of product 1
    user_inputs = ['1'] * products[0].units  # Purchase all units
    user_inputs += ['1']  # Attempt to purchase one more
    user_inputs += ['c', 'y', 'l', 'y']
    total_price = products[0].units * product_prices['1']
    expected_wallet = 5000.0 - total_price
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC5: 购物车操作      Cart operations
##############################

def test_view_cart_empty(monkeypatch):
    """TC15: view cart when empty"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['c', 'n', 'l', 'y']  # 查看空购物车，退出
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_view_cart_with_items(monkeypatch):
    """TC16: view cart when items in cart"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['1', 'c', 'n', 'n', 'l', 'y']  # 添加商品，查看购物车，退出
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_buy_and_remove_item(monkeypatch):
    """TC17: buy and remove item"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = [
        '1',        # 添加商品1
        'c',        # 查看购物车
        'n',        # 不结账
        'y',        # 是否移除商品？是
        '1',        # 移除商品1
        'c',        # 查看购物车
        'n',        # 不结账
        'n',        # 不移除商品
        'l', 'y'    # 退出
    ]
    expected_wallet = 500.0  # 未购买任何商品
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_remove_nonexistent_item(monkeypatch):
    """TC18: attempt to remove item not in cart"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = [
        'c',        # 查看购物车（空）
        'n',        # 不结账
        'y',        # 是否移除商品？是
        '1',        # 尝试移除不存在的商品
        'l', 'y'    # 退出
    ]
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC6: 结账过程      Checkout
##############################

def test_checkout_empty_cart(monkeypatch):
    """TC19: checkout with empty cart"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['c', 'y', 'l', 'y']  # 尝试结账空购物车，退出
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_checkout_with_items_sufficient_funds(monkeypatch):
    """TC20: checkout with items, sufficient funds"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['1', 'c', 'y', 'l', 'y']  # 添加商品，结账，退出
    expected_wallet = 500.0 - product_prices['1']
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_checkout_with_items_insufficient_funds(monkeypatch):
    """TC21: checkout with items, insufficient funds"""
    login_info = {'username': 'user1', 'wallet': 1.0}
    user_inputs = ['6', 'c', 'y', 'n', 'n', 'l', 'y']  # 尝试购买超出钱包的商品
    expected_wallet = 1.0  # 钱包余额不变
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC7: 用户操作       Do nothing
##############################

def test_logout_immediately_after_login(monkeypatch):
    """TC22: logout immediately after login"""
    login_info = {'username': 'user1', 'wallet': 100.0}
    user_inputs = ['l', 'y']
    expected_wallet = 100.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_logout_cancel(monkeypatch):
    """TC23: attempt to logout but cancel"""
    login_info = {'username': 'user1', 'wallet': 100.0}
    user_inputs = ['l', 'n', 'l', 'y']
    expected_wallet = 100.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

##############################
# 等价类 EC25 - EC28: 其他工作流程
##############################

def test_buy_remove_buy_again(monkeypatch):
    """TC24: buy, remove, and buy again"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = [
        '1',        # 添加商品1
        'c',        # 查看购物车
        'n',        # 不结账
        'y',        # 是否移除商品？是
        '1',        # 移除商品1
        '1',        # 再次添加商品1
        'c',        # 查看购物车
        'y',        # 结账
        'l', 'y'    # 退出
    ]
    expected_wallet = 500.0 - product_prices['1']
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_view_products(monkeypatch):
    """TC25: view products"""
    login_info = {'username': 'user1', 'wallet': 500.0}
    user_inputs = ['d', 'l', 'y']  # 显示产品列表，退出
    expected_wallet = 500.0
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)

def test_multiple_checkouts(monkeypatch):
    """TC26: multiple checkouts in one session"""
    login_info = {'username': 'user1', 'wallet': 1000.0}
    user_inputs = [
        '1', 'c', 'y',   # 第一次购买并结账
        '2', 'c', 'y',   # 第二次购买并结账
        'l', 'y'         # 退出
    ]
    total_price = product_prices['1'] + product_prices['2']
    expected_wallet = 1000.0 - total_price
    run_checkout_and_payment_test(monkeypatch, login_info, user_inputs, expected_wallet)
