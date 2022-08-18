from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException, WebDriverException
from selenium.webdriver.chrome.options import Options
import pickle 
import pandas as pd
from tqdm import tqdm


def run_scraping(search_key, implicitly_wait_time = 5, headless=True, disabletqdm = False, windows=True):
    url = "https://apps.tga.gov.au/prod/DEVICES/daen-report.aspx"

    # create a new Chrome session
    chrome_options = Options()
    if headless:
        chrome_options.add_argument("--headless")
    try:
        if windows:
            driver = webdriver.Chrome('./chromedriver_win32', options=chrome_options)
        else:
            driver = webdriver.Chrome('./chromedriver', options=chrome_options)

    except WebDriverException:
        driver = webdriver.Chrome('tgaScraping/chromedriver',  options=chrome_options)

    driver.implicitly_wait(implicitly_wait_time) # Set implicit wait time
    driver.get(url)

    accept_button = driver.find_element_by_id("disclaimer-accept")
    accept_button.click()

    medicinename = driver.find_element_by_id("medicine-name")
    medicinename.send_keys(search_key)

    medicines_check_all = driver.find_element_by_class_name('medicines-check-all')
    medicines_check_all.click()

    submit_button =  driver.find_element_by_id('submit-button')
    submit_button.click()

    # Click show all results
    records_to_display = driver.find_element_by_name('ctl00$body$MedicineSummaryControl$cmbPageSelection')
    for option in records_to_display.find_elements_by_tag_name('option'):
        if option.text == 'All':
            option.click() # select() in earlier versions of webdriver
            break


    # Get all Medical device summary
    try:
        print('Saving medical device summary.')
        table = driver.find_element_by_id("ctl00_body_MedicineSummaryControl_grdSummary")
        medical_device_summary = []
        driver.implicitly_wait(1)
        for i, row in enumerate(tqdm(table.find_elements_by_css_selector('tr'), disable=disabletqdm)):
            if i == 0:
                header_text = [d.text for d in row.find_elements_by_css_selector('th')]
            else:
                medical_device_summary.append([d.text for d in row.find_elements_by_css_selector('td')])
        medical_device_summary = pd.DataFrame(medical_device_summary, columns = header_text)
        medical_device_summary.to_csv('../data/medical_device_summary_key_%s.csv' % search_key, index = False)
    except:
        print('Failed to load medical devices data')

    # Move to list of reports
    list_of_reports_button = driver.find_element_by_id('ctl00_body_TabPCL')
    list_of_reports_button.click()
    driver.implicitly_wait(10)

    # Click show all results
    records_to_display = driver.find_element_by_name('ctl00$body$CaseListingControl$cmbPageSelection')
    # ('ctl00_body_CaseListingControl_cmbPageSelection')
    for option in records_to_display.find_elements_by_tag_name('option'):
        if option.text == 'All':
            option.click() # select() in earlier versions of webdriver
            break

    
    results = driver.find_element_by_id('ctl00_body_CaseListingControl_UpdatePanelPCL')
    modules = results.find_elements_by_class_name('moduleBody')

    print('Saving list of reports.')
    driver.implicitly_wait(0.05)
    all_module_data = []
    for module in tqdm(modules, disable=disabletqdm):
        # Get ordered headings
        headings_elements = module.find_elements_by_class_name('fieldHeading')
        headings = [heading.text for heading in headings_elements]
        headings.remove('Event description')
        headings = headings[:-2]

        # Get ordered text
        text_elements = module.find_elements_by_css_selector('.column-38')
        text = [t.text.rstrip() for t in text_elements]
        text = text[:-1]
        module_data = dict(zip(headings,text))

        # Get event description separately
        rows = module.find_elements_by_class_name('columns') # I recognize this is weird
        record_desc_flag = False 
        event_description = ""
        for r in rows:
            if 'Reported event outcome' in r.text: # Break after finish
                break
            if record_desc_flag:  # While in between, keep recording description
                event_description += r.text + "  "
            if r.text == 'Event description': # Start recording after title
                record_desc_flag = True
        module_data['Event description']=event_description
        # Get other medical devices reported as being used
        try: 
            table = module.find_element_by_class_name('table-responsive') # Search is probably slowing something down
            columns_titles_element  = table.find_element_by_class_name('otherDevicesHeading')
            table_data = []
            for i, row in enumerate(table.find_elements_by_css_selector('tr')):
                if i ==0:
                    columns_titles = [t.text for t in columns_titles_element.find_elements_by_css_selector('th')]
                else:
                    table_data.append(dict(zip(columns_titles, [t.text for t in row.find_elements_by_css_selector('td')])))
        except NoSuchElementException:
            table_data = None
        module_data['other medical devices reported as being used'] = table_data

        all_module_data.append(module_data)

    with open("../data/list_of_reports_%s.pickle" % search_key, "wb") as f:
        pickle.dump(all_module_data, f, pickle.HIGHEST_PROTOCOL)
    