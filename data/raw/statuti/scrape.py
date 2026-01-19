from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait, Select
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.chrome.service import Service
import time
import pandas as pd
import logging
import os
import json
import re
from datetime import datetime

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

class MedievalStatutesScraper:
    def __init__(self, headless=False, resume_file='scraper_progress.json'):
        self.options = webdriver.ChromeOptions()
        if headless:
            self.options.add_argument('--headless')
        self.options.add_argument('--no-sandbox')
        self.options.add_argument('--disable-dev-shm-usage')
        self.options.add_argument('--disable-gpu')
        # Add performance optimizations
        self.options.add_argument('--disable-images')
        self.options.add_argument('--disable-javascript')  # Remove if site needs JS
        self.options.add_argument('--disable-plugins')
        self.options.add_argument('--disable-extensions')
        
        self.driver = None
        self.data = []
        self.resume_file = resume_file
        self.processed_indices = set()
    
    def extract_earliest_year(self, text):
        """Extract all 4-digit numbers from text and return the smallest one"""
        if not text:
            return None
        
        # Find all 4-digit numbers in the text
        years = re.findall(r'\b\d{4}\b', text)
        
        if not years:
            return None
        
        # Convert to integers and return the smallest
        year_ints = [int(y) for y in years]
        earliest = min(year_ints)
        
        logger.info(f"Found years {years} in text, using earliest: {earliest}")
        return earliest
        
    def save_progress(self, current_index, total_options):
        """Save current progress to resume later"""
        progress_data = {
            'current_index': current_index,
            'total_options': total_options,
            'processed_indices': list(self.processed_indices),
            'data': self.data,
            'timestamp': datetime.now().isoformat()
        }
        with open(self.resume_file, 'w', encoding='utf-8') as f:
            json.dump(progress_data, f, indent=2)
        logger.info(f"Progress saved at index {current_index}")
    
    def load_progress(self):
        """Load previous progress if exists"""
        if os.path.exists(self.resume_file):
            try:
                with open(self.resume_file, 'r', encoding='utf-8') as f:
                    progress_data = json.load(f)
                self.data = progress_data.get('data', [])
                self.processed_indices = set(progress_data.get('processed_indices', []))
                current_index = progress_data.get('current_index', 0)
                total_options = progress_data.get('total_options', 0)
                timestamp = progress_data.get('timestamp', 'unknown')
                logger.info(f"Resumed from index {current_index}/{total_options} (saved: {timestamp})")
                logger.info(f"Already processed {len(self.processed_indices)} items")
                return current_index, total_options
            except Exception as e:
                logger.error(f"Failed to load progress: {e}")
        return 0, 0

    def start_driver(self):
        try:
            service = Service(ChromeDriverManager().install())
            self.driver = webdriver.Chrome(service=service, options=self.options)
            self.driver.maximize_window()
            # Reduce implicit wait time for faster failures
            self.driver.implicitly_wait(2)
            logger.info("Chrome driver started successfully")
        except Exception as e:
            logger.error(f"Failed to start Chrome driver: {e}")
            raise

    def navigate_to_site(self, url):
        try:
            self.driver.get(url)
            WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.NAME, "localita"))
            )
            logger.info("Successfully navigated to the site")
        except TimeoutException:
            logger.error("Timeout waiting for page to load")
            raise

    def get_dropdown_options_count(self):
        """Get total number of dropdown options"""
        try:
            dropdown_element = WebDriverWait(self.driver, 10).until(
                EC.presence_of_element_located((By.NAME, "localita"))
            )
            select = Select(dropdown_element)
            options = select.options[1:]  # Skip default/empty option
            total_options = len(options)
            logger.info(f"Found {total_options} dropdown options")
            return total_options
        except NoSuchElementException:
            logger.error("Dropdown element not found")
            raise

    def get_dropdown_option_info(self, index):
        """Get option info by index - fresh lookup each time"""
        max_retries = 3
        for attempt in range(max_retries):
            try:
                dropdown_element = WebDriverWait(self.driver, 5).until(
                    EC.presence_of_element_located((By.NAME, "localita"))
                )
                select = Select(dropdown_element)
                options = select.options[1:]
                if index >= len(options):
                    raise IndexError(f"Index {index} out of range for {len(options)} options")
                option = options[index]
                value = option.get_attribute('value')
                text = option.text
                return value, text
            except Exception as e:
                logger.warning(f"Attempt {attempt + 1} failed: {e}")
                if attempt == max_retries - 1:
                    raise
                time.sleep(0.25)

    def select_dropdown_option_by_index(self, index):
        """Select dropdown option by index with multiple fallback methods"""
        max_retries = 3
        for attempt in range(max_retries):
            try:
                time.sleep(0.1)
                dropdown_element = WebDriverWait(self.driver, 5).until(
                    EC.element_to_be_clickable((By.NAME, "localita"))
                )
                select = Select(dropdown_element)
                options = select.options[1:]
                if index >= len(options):
                    raise IndexError(f"Index {index} out of range")
                target_option = options[index]
                option_value = target_option.get_attribute('value')
                option_text = target_option.text

                # Try multiple selection methods
                for method in [
                    lambda: select.select_by_index(index + 1),
                    lambda: select.select_by_value(option_value),
                    lambda: select.select_by_visible_text(option_text)
                ]:
                    try:
                        method()
                        break
                    except Exception:
                        continue

                time.sleep(0.1)
                dropdown_element = WebDriverWait(self.driver, 5).until(
                    EC.presence_of_element_located((By.NAME, "localita"))
                )
                select = Select(dropdown_element)
                selected_option = select.first_selected_option
                selected_text = selected_option.text
                selected_value = selected_option.get_attribute('value')

                logger.info(f"Selected dropdown option: {selected_text}")
                return selected_value, selected_text
            except Exception as e:
                logger.warning(f"Attempt {attempt + 1} failed to select: {e}")
                if attempt == max_retries - 1:
                    raise
                time.sleep(0.25)

    def wait_for_results_to_load(self):
        try:
            wait = WebDriverWait(self.driver, 5)
            indicators = [
                (By.CSS_SELECTOR, "a[href*='OpenDocument']"),
                (By.CSS_SELECTOR, "table a"),
                (By.CSS_SELECTOR, "tbody a"),
                (By.XPATH, "//a[contains(text(), 'Badia') or contains(text(), 'Statut')]")
            ]
            for locator in indicators:
                try:
                    wait.until(EC.presence_of_element_located(locator))
                    logger.info("Results loaded")
                    return True
                except TimeoutException:
                    continue
            logger.warning("No results found or loaded")
            return False
        except Exception as e:
            logger.error(f"Error waiting for results: {e}")
            return False

    def click_first_result_link(self):
        """Extract href and text of the first result without opening it"""
        try:
            selectors = [
                "a[href*='OpenDocument']",
                "table a[href*='OpenDocument']",
                "tbody a[href*='OpenDocument']",
                "tr a[href*='OpenDocument']",
                "a[href*='Badia']",
                "table a",
                "tbody a"
            ]
            for selector in selectors:
                try:
                    links = self.driver.find_elements(By.CSS_SELECTOR, selector)
                    if links:
                        link_href = links[0].get_attribute('href')
                        link_text = links[0].text
                        logger.info(f"Found first result link: {link_text} -> {link_href}")
                        return link_href, link_text
                except Exception:
                    continue
            logger.warning("No result links found")
            return None, None
        except Exception as e:
            logger.error(f"Failed to extract link: {e}")
            return None, None

    def scrape_all_data(self, base_url):
        try:
            # Load previous progress
            start_index, saved_total = self.load_progress()
            
            self.start_driver()
            self.navigate_to_site(base_url)
            total_options = self.get_dropdown_options_count()

            if saved_total > 0 and saved_total != total_options:
                logger.warning(f"Option count changed: saved={saved_total}, current={total_options}")

            logger.info(f"Processing {total_options} options, starting from index {start_index}")

            for i in range(total_options):
                if i in self.processed_indices:
                    logger.info(f"Skipping already processed option {i + 1}/{total_options}")
                    continue
                    
                if i < start_index:
                    continue

                logger.info(f"Processing option {i + 1}/{total_options}")
                try:
                    # Always refresh page to avoid stale elements
                    self.navigate_to_site(base_url)
                    option_value, option_text = self.get_dropdown_option_info(i)
                    selected_value, selected_text = self.select_dropdown_option_by_index(i)

                    if self.wait_for_results_to_load():
                        link_href, link_text = self.click_first_result_link()
                        earliest_year = self.extract_earliest_year(link_text)
                        
                        # Enhanced progress reporting with year
                        year_display = f"Year: {earliest_year}" if earliest_year else "Year: None"
                        logger.info(f"✓ Option {i + 1}/{total_options} - {selected_text} - {year_display}")
                        
                        self.data.append({
                            'dropdown_value': selected_value,
                            'dropdown_text': selected_text,
                            'document_link': link_href,
                            'link_text': link_text,
                            'earliest_year': earliest_year
                        })
                    else:
                        logger.info(f"✗ Option {i + 1}/{total_options} - {selected_text} - No results")
                        self.data.append({
                            'dropdown_value': selected_value,
                            'dropdown_text': selected_text,
                            'document_link': None,
                            'link_text': None,
                            'earliest_year': None
                        })
                except Exception as e:
                    logger.error(f"Error processing option {i + 1}: {e}")
                    try:
                        # Try to get option info even if selection failed
                        self.navigate_to_site(base_url)
                        option_value, option_text = self.get_dropdown_option_info(i)
                    except:
                        option_value, option_text = f"index_{i}", f"unknown_option_{i}"
                    self.data.append({
                        'dropdown_value': option_value,
                        'dropdown_text': option_text,
                        'document_link': None,
                        'link_text': None,
                        'earliest_year': None
                    })
                
                self.processed_indices.add(i)
                
                # Save progress every 5 items
                if (i + 1) % 5 == 0:
                    self.save_progress(i + 1, total_options)
                
                # Brief pause between requests
                time.sleep(0.1)

            # Final save only if we actually completed all items
            if len(self.processed_indices) >= total_options:
                self.save_progress(total_options, total_options)
                logger.info(f"Scraping completed. Collected {len(self.data)} records")
            else:
                logger.info(f"Scraping session ended. Processed {len(self.processed_indices)}/{total_options} items")

        except KeyboardInterrupt:
            logger.info("Scraping interrupted by user")
            if hasattr(self, 'processed_indices') and self.processed_indices:
                last_processed = max(self.processed_indices) if self.processed_indices else 0
                self.save_progress(last_processed + 1, total_options)
            raise
        except Exception as e:
            logger.error(f"Scraping error: {e}")
            raise
        finally:
            if self.driver:
                self.driver.quit()

    def save_data(self, filename='medieval_statutes_data.csv'):
        if self.data:
            df = pd.DataFrame(self.data)
            # Add timestamp to filename to avoid overwrites
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            filename_with_timestamp = f"{filename.split('.')[0]}_{timestamp}.csv"
            df.to_csv(filename_with_timestamp, index=False, encoding='utf-8')
            logger.info(f"Data saved to {filename_with_timestamp}")
            return df
        else:
            logger.warning("No data to save")
            return None

    def clean_progress_file(self):
        """Remove progress file after successful completion"""
        try:
            if os.path.exists(self.resume_file):
                os.remove(self.resume_file)
                logger.info("Progress file cleaned up")
        except Exception as e:
            logger.error(f"Failed to clean progress file: {e}")

def main():
    base_url = "https://www.senato.it/w3/Biblioteca/catalogoDegliStatutiMedievali.nsf/SchedeLocalita?OpenForm"
    scraper = MedievalStatutesScraper(headless=True)
    completed_successfully = False

    try:
        scraper.scrape_all_data(base_url)
        completed_successfully = True
        df = scraper.save_data()
        if df is not None:
            print("\nSample of collected data:")
            print(df.head())
            print(f"\nTotal records collected: {len(df)}")
            print(f"Records with links: {df['document_link'].notna().sum()}")
            print(f"Records with years: {df['earliest_year'].notna().sum()}")
            
            # Show year statistics
            if df['earliest_year'].notna().sum() > 0:
                print(f"\nYear statistics:")
                print(f"  Earliest year found: {df['earliest_year'].min()}")
                print(f"  Latest year found: {df['earliest_year'].max()}")
                print(f"  Average year: {df['earliest_year'].mean():.0f}")
            
    except KeyboardInterrupt:
        print(f"\n=== SCRIPT INTERRUPTED ===")
        print(f"Progress has been saved.")
        print(f"Run the script again to resume from where you left off.")
        return
        
    except Exception as e:
        logger.error(f"Script failed: {e}")
        print(f"\nScript encountered an error but progress was saved.")
        print(f"Run the script again to resume from where you left off.")
        return
    
    # Only clean up progress file if we actually completed successfully
    if completed_successfully:
        scraper.clean_progress_file()
        print(f"\n=== SCRAPING COMPLETED SUCCESSFULLY ===")
        print(f"Progress file cleaned up.")

if __name__ == "__main__":
    main()