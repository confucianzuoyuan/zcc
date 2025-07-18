#include <stdio.h>
#include <pthread.h>
#include <stdatomic.h>
#include <unistd.h>

volatile int lock = 0;

void lock_acquire(volatile int *lock) {
    while (__sync_lock_test_and_set(lock, 1)) {
        // 自旋等待锁释放
        while (*lock) {
            // CPU放松指令，避免过度忙等（可选）
            __asm__ __volatile__("pause");
        }
    }
}

void lock_release(volatile int *lock) {
    __sync_lock_release(lock); // 原子释放锁，写0并带内存屏障
}

void *worker(void *arg) {
    printf("线程 %ld 尝试获取锁\n", (long)arg);
    lock_acquire(&lock);
    printf("线程 %ld 获得锁\n", (long)arg);

    // 模拟临界区工作
    sleep(1);

    printf("线程 %ld 释放锁\n", (long)arg);
    lock_release(&lock);
    return NULL;
}

int main() {
    pthread_t t1, t2;

    pthread_create(&t1, NULL, worker, (void *)1);
    sleep(0.1); // 让线程1先运行
    pthread_create(&t2, NULL, worker, (void *)2);

    pthread_join(t1, NULL);
    pthread_join(t2, NULL);

    printf("测试结束\n");
    return 0;
}
