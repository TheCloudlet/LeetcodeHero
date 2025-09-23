#include <cstdarg>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <mutex>
#include <string>

class CloudletLogger {
public:
    static CloudletLogger& getInstance() {
        static CloudletLogger instance;
        return instance;
    }

    void logf(const char* format, ...) {
        constexpr size_t bufferSize = 1024;
        char buffer[bufferSize];

        va_list args;
        va_start(args, format);
        vsnprintf(buffer, bufferSize, format, args);
        va_end(args);

        std::lock_guard<std::mutex> lock(mutex);  // Thread-safe logging
        // Log the message to a file or console
        std::cout << buffer << std::endl;

        if (file.is_open()) {
            file << buffer << std::endl;
        }
    }

private:
    CloudletLogger() {
        file.open(logFile);
    }

    ~CloudletLogger() {
        if (file.is_open()) {
            file.close();
        }
    }

    // Delete copy constructor and assignment operator
    CloudletLogger(const CloudletLogger&) = delete;
    CloudletLogger& operator=(const CloudletLogger&) = delete;

    std::ofstream file;
    std::string logFile = "cloudlet_log.txt";
    std::mutex mutex;
};
