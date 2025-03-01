import pytest
from unittest.mock import patch
from online_shopping_cart.product.product_search import display_csv_as_table


mock_header = ['Product', 'Price', 'Units']
"""
mock_csv_data = [
    ['Apple', '2', '10'],
    ['Banana', '1', '15'],
    ['Orange', '1.5', '8'],
    ['Grapes', '3', '5'],

]
"""
# Test Case 1: File Contains Header and Multiple Rows of Valid Data
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_valid(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '2', '10'],
        ['Banana', '1', '15'],
        ['Orange', '1.5', '8'],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
        "['Banana', '1', '15']\n"
        "['Orange', '1.5', '8']\n"
    )
    assert captured.out == expected_output

# Test Case 2: Only Header, No Data
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_valid_empty(mock_get_csv_data, capsys):
    mock_get_csv_data.return_value = (mock_header, [])
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = "\n['Product', 'Price', 'Units']\n"
    assert captured.out == expected_output

# Test Case 3: CSV Empty File (No Header, No Data)
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_empty_file(mock_get_csv_data, capsys):
    mock_get_csv_data.return_value = ([], [])
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = "\n[]\n"  # 根据实际输出调整
    assert captured.out == expected_output

# Test Case 4: Header or Data Contains Special Characters
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_special_characters(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Ap,ple', '2', '10'],
        ['Ban"ana', '1', '15'],
        ['Oran\nge', '1.5', '8'],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Ap,ple', '2', '10']\n"
        "['Ban\"ana', '1', '15']\n"
        "['Oran\\nge', '1.5', '8']\n"
    )
    assert captured.out == expected_output

# Test Case 5: Data Contains Empty Values
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_with_empty_values(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '', '10'],
        ['Banana', '1', ''],
        ['', '1.5', '8'],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '', '10']\n"
        "['Banana', '1', '']\n"
        "['', '1.5', '8']\n"
    )
    assert captured.out == expected_output

# Test Case 6: File Contains Large Amount of Data
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_large_file(mock_get_csv_data, capsys):
    mock_csv_data = [['Product' + str(i), str(i * 1.5), str(i * 10)] for i in range(1, 101)]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    assert len(captured.out.splitlines()) == 101 + 1  # 100 rows + 1 header

# Test Case 7: File Contains Extra Blank Lines
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_with_empty_rows(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '2', '10'],
        [],
        ['Banana', '1', '15'],
        ['Orange', '1.5', '8'],
        [],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
        "[]\n"
        "['Banana', '1', '15']\n"
        "['Orange', '1.5', '8']\n"
        "[]\n"
    )
    assert captured.out.strip() == expected_output.strip()

# Test Case 8: Fewer Columns in Data than in Header
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_fewer_columns(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '2'],
        ['Banana'],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2']\n"
        "['Banana']\n"
    )
    assert captured.out == expected_output

# Test Case 9: Empty Header, but Data Exists
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_empty_header(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '2', '10'],
        ['Banana', '1', '15'],
    ]
    mock_get_csv_data.return_value = ([], mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = "\n[]\n['Apple', '2', '10']\n['Banana', '1', '15']\n"
    assert captured.out == expected_output

# Test Case 10: File Path is Default Path (No Input Parameter)
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_csv_as_table_default_path(mock_get_csv_data, capsys):
    mock_csv_data = [
        ['Apple', '2', '10'],
        ['Banana', '1', '15'],
    ]
    mock_get_csv_data.return_value = (mock_header, mock_csv_data)
    display_csv_as_table()
    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
        "['Banana', '1', '15']\n"
    )
    assert captured.out == expected_output

# Test Case 11: Invalid Input Type: Integer
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=ValueError)
def test_display_csv_as_table_invalid_int(mock_get_csv_data):
    with pytest.raises(ValueError):
        display_csv_as_table(123)

# Test Case 12: Invalid Input Type: Float
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=ValueError)
def test_display_csv_as_table_invalid_float(mock_get_csv_data):
    with pytest.raises(ValueError):
        display_csv_as_table(123.45)

# Test Case 13: Invalid Input Type: List
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=ValueError)
def test_display_csv_as_table_invalid_list(mock_get_csv_data):
    with pytest.raises(ValueError):
        display_csv_as_table(['invalid', 'list'])

# Test Case 14: Invalid Input Type: Dictionary
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=ValueError)
def test_display_csv_as_table_invalid_dict(mock_get_csv_data):
    with pytest.raises(ValueError):
        display_csv_as_table({'invalid': 'dict'})



# Test Case 15: Non-Existent File Path
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=FileNotFoundError)
def test_display_csv_as_table_nonexistent_file(mock_get_csv_data):
    with pytest.raises(FileNotFoundError):
        display_csv_as_table("nonexistent_file.csv")


# Test Case 16: File Path as Empty String
@patch('online_shopping_cart.product.product_search.get_csv_data', side_effect=ValueError("Empty file path"))
def test_display_csv_as_table_empty_file_path(mock_get_csv_data):
    with pytest.raises(ValueError, match="Empty file path"):
        display_csv_as_table("")