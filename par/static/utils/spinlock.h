/*
 * spinlock.h
 *
 *  Created on: Mar 1, 2015
 *      Author: kp
 */

#ifndef UTILS_SPINLOCK_H_
#define UTILS_SPINLOCK_H_


#include <atomic>


namespace utils {


class SpinLock {
private:
    std::atomic<bool> locked;
public:
    SpinLock() : locked(false) {}

    void lock() {
    	while ( locked.exchange(true) ) {}
    }

    void unlock() {
    	locked.store(false);
    }
};


}

#endif /* UTILS_SPINLOCK_H_ */
