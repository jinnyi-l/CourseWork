import pytest
from unittest.mock import patch, MagicMock
from online_shopping_cart.product.product_search import display_filtered_table, display_csv_as_table

PRODUCTS_FILE_PATHNAME = "products.csv"
PRODUCT_HEADER_INDEX = "Product"  # 根据 CSV 文件内容
#1.Test when search_target is None, display_csv_as_table should be called.
@patch('online_shopping_cart.product.product_search.display_csv_as_table')
def test_display_filtered_table_no_search_target(mock_display_csv_as_table):

    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=None)
    mock_display_csv_as_table.assert_called_once_with(csv_file_name=PRODUCTS_FILE_PATHNAME)

#2.Test with a valid search_target that matches some rows.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_with_matches(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
            ["Strawberry", "4", "12"],
            ["Watermelon", "10", "1"],
            ["Carrot", "0.5", "20"],
        ],
    )
    search_target = "Apple"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
    )
    assert captured.out == expected_output

#3.Test with a valid search_target but no matching rows.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_no_matches(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
            ["Strawberry", "4", "12"],
            ["Watermelon", "10", "1"],
            ["Carrot", "0.5", "20"],
        ],
    )
    search_target = "Mango"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = "\n['Product', 'Price', 'Units']\n"
    assert captured.out == expected_output

#4.Test with an empty file (no rows).
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_empty_file(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (["Product", "Price", "Units"], [])  # Only header, no rows
    search_target = "Apple"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = "\n['Product', 'Price', 'Units']\n"
    assert captured.out == expected_output

#5.Test with a non-existent file.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_file_not_found(mock_get_csv_data):

    mock_get_csv_data.side_effect = FileNotFoundError("File not found")

    with pytest.raises(FileNotFoundError, match="File not found"):
        display_filtered_table(csv_file_name="non_existent.csv", search_target="Apple")


# 6. Test with an empty search_target.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_empty_search_target(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )
    search_target = ""
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"

    )
    assert captured.out == expected_output

# 7. Test Case-Insensitive Matching
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_case_insensitive(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )
    search_target = "apple"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
    )
    assert captured.out == expected_output


# 8. Test with search_target that matches multiple rows.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_multiple_matches(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Strawberry", "4", "12"],
            ["Carrot", "0.5", "20"],
            ["apple", "2", "10"],  # Another matching result for "apple"
        ],
    )
    search_target = "apple"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"
        "['apple', '2', '10']\n"
    )
    assert captured.out == expected_output


# 9. Test with None as search_target, expecting display_csv_as_table to be called.
@patch('online_shopping_cart.product.product_search.get_csv_data')
@patch('online_shopping_cart.product.product_search.display_csv_as_table')
def test_display_filtered_table_search_target_none(mock_display_csv_as_table, mock_get_csv_data):

    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=None)
    mock_display_csv_as_table.assert_called_once_with(csv_file_name=PRODUCTS_FILE_PATHNAME)

#10. Test with a search_target that partially matches some data entries.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_partial_match(mock_get_csv_data, capsys):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],  # Header
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
            ["Pineapple", "5", "3"],
        ],
    )
    search_target = "apple"  # Partial match with "Apple" and "Pineapple"
    display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

    captured = capsys.readouterr()
    expected_output = (
        "\n['Product', 'Price', 'Units']\n"
        "['Apple', '2', '10']\n"

    )
    assert captured.out == expected_output



#11. Test with an int as search_target, expecting an AttributeError.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_non_string_search_target(mock_get_csv_data):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )

    search_target = 123

    # Assert that AttributeError is raised when non-string search_target is passed
    with pytest.raises(AttributeError, match="'int' object has no attribute 'capitalize'"):
        display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

#12. Test with a float as search_target, expecting an AttributeError.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_non_string_search_target_float(mock_get_csv_data):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )
    search_target = 123.45
    with pytest.raises(AttributeError, match="'float' object has no attribute 'capitalize'"):
        display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

#13. Test with a dictionary as search_target, expecting an AttributeError.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_non_string_search_target_dict(mock_get_csv_data):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )
    search_target = {"product": "Apple"}
    with pytest.raises(AttributeError, match="'dict' object has no attribute 'capitalize'"):
        display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)

#14. Test with a boolean as search_target, expecting an AttributeError.
@patch('online_shopping_cart.product.product_search.get_csv_data')
def test_display_filtered_table_non_string_search_target_bool(mock_get_csv_data):

    mock_get_csv_data.return_value = (
        ["Product", "Price", "Units"],
        [
            ["Apple", "2", "10"],
            ["Banana", "1", "15"],
            ["Orange", "1.5", "8"],
            ["Grapes", "3", "5"],
        ],
    )
    search_target = True
    with pytest.raises(AttributeError, match="'bool' object has no attribute 'capitalize'"):
        display_filtered_table(csv_file_name=PRODUCTS_FILE_PATHNAME, search_target=search_target)


